# Performance Improvement Opportunities

This document describes actionable performance improvements for the JuicyPixels-webp codec. Improvements are prioritized by impact and implementation effort.

## Summary

| Improvement | Files | Est. Impact | Effort | Priority |
|-------------|-------|-------------|--------|----------|
| **Color cache mutable refactor** | LZ77.hs | **30-50%** | Medium | **CRITICAL** |
| **LZ77 copy loop batching** | LZ77.hs | **15-25%** | Medium | **CRITICAL** |
| BitReader fast paths | BitReader.hs | 8-12% | Low | HIGH |
| Loop filter interior fast path | LoopFilter.hs | 5-8% | Medium | HIGH |
| Transform fusion | Transform.hs | 12-18% | Medium | HIGH |
| Transform neighbor caching | Transform.hs | 8-12% | Medium | HIGH |
| Integer → Int arithmetic | LZ77.hs, Transform.hs | 5-10% | Low | HIGH |
| Prefix code specialization | PrefixCode.hs | 6-10% | Medium | MEDIUM |
| LZ77 copy unrolling | LZ77.hs | 8-15% | Low | MEDIUM |
| Mode selection batching | ModeSelection.hs | 15-25% | Medium | MEDIUM |
| Huffman sort optimization | EncodeComplete.hs | 5-10% | Low | MEDIUM |
| Predictor mode early exit | PredictorEncode.hs | 10-15% | Low | MEDIUM |
| IDCT/DCT optimization | IDCT.hs, DCT.hs | 3-5% | Low | LOW |
| Huffman table build | PrefixCode.hs | 8-12% | High | LOW |
| INLINE pragmas | Multiple | 2-5% | Low | LOW |

**Overall potential gains:**
- VP8L decoding: 50-80% faster (with CRITICAL fixes)
- VP8 lossy decoding: 15-25% faster
- VP8L encoding: 30-50% faster
- VP8 lossy encoding: 15-20% faster

---

## CRITICAL Priority

These are the highest-impact optimizations. They address fundamental allocation and loop inefficiencies in the hottest code paths.

### 1. Color Cache Mutable Refactor

**File:** `src/Codec/Picture/WebP/Internal/VP8L/LZ77.hs:40-44`

**Issue:** The color cache uses immutable vectors with `VS.modify`, which allocates a **new vector on every pixel**:

```haskell
-- Current (lines 40-44):
insertColor :: Word32 -> ColorCache -> ColorCache
insertColor color cache =
  let idx = colorCacheHash color (ccBits cache)
      newColors = VS.modify (\v -> VSM.write v idx color) (ccColors cache)
   in cache {ccColors = newColors}
```

This is called once per pixel (literal, back-reference copy, or cache lookup) — millions of times per image. Each call:
1. Thaws the immutable vector to mutable
2. Writes one element
3. Freezes back to immutable
4. Creates a new `ColorCache` record

**Solution:** Thread a mutable vector through the decode loop in ST monad:

```haskell
-- Proposed:
data MutableColorCache s = MutableColorCache
  { mccBits :: !Int,
    mccColors :: !(VSM.MVector s Word32)
  }

insertColorM :: Word32 -> MutableColorCache s -> ST s ()
insertColorM color cache = do
  let idx = colorCacheHash color (mccBits cache)
  VSM.write (mccColors cache) idx color

-- In decodeLZ77:
decodeLZ77 ... = runST $ do
  cache <- createMutableColorCache bits
  -- pass cache through loop, call insertColorM directly
```

**Impact:** 30-50% speedup on VP8L decode for images with color cache enabled.

---

### 2. LZ77 Copy Loop Batching

**File:** `src/Codec/Picture/WebP/Internal/VP8L/LZ77.hs:330-346`

**Issue:** Back-reference copying is done pixel-by-pixel with recursive calls:

```haskell
-- Current (lines 330-346):
copyLoop !pos !dist !len !out !cache !maybeC !r
  | len <= 0 = loop pos cache r
  | pos >= totalPixels = loop pos cache r
  | otherwise = do
      let srcPos = pos - dist
      color <- VSM.read out srcPos    -- 1 read per pixel
      VSM.write out pos color         -- 1 write per pixel
      let cache' = case maybeC of
            Nothing -> cache
            Just c -> insertColor color c  -- allocation per pixel!
      copyLoop (pos + 1) dist (len - 1) out cache' maybeC r  -- recursive call
```

For a 100-pixel back-reference, this makes 100 recursive calls, 100 reads, 100 writes, and potentially 100 cache insertions.

**Solution:** Batch operations and special-case common patterns:

```haskell
copyLoop !pos !dist !len !out !cache !r
  | len <= 0 = loop pos cache r
  | otherwise = do
      -- Special case: dist=1 means repeat single pixel
      if dist == 1
        then do
          color <- VSM.read out (pos - 1)
          forM_ [pos .. pos + len - 1] $ \i -> VSM.write out i color
          -- Only insert final color to cache (sampling)
          insertColorM color cache
        else if dist >= len
          then do
            -- Non-overlapping: use bulk copy
            let src = VSM.slice (pos - dist) len out
            let dst = VSM.slice pos len out
            VSM.copy dst src
            -- Sample cache insertions (every 8th pixel)
            forM_ [0, 8 .. len - 1] $ \i -> do
              c <- VSM.read out (pos + i)
              insertColorM c cache
          else
            -- Overlapping: must copy sequentially (but batch reads/writes)
            copyOverlapping pos dist len out cache
      loop (pos + len) cache r
```

**Impact:** 15-25% speedup on back-reference heavy images.

---

## HIGH Priority

### 3. BitReader Fast Paths

**File:** `src/Codec/Picture/WebP/Internal/BitReader.hs`

**Issue:** BitReader is called millions of times per image. The current implementation:
- Uses general pattern matching even for single-bit reads (most common case)
- Recursive refillBuffer logic adds overhead
- Performs mask+shift on every `readBits` call

**Improvements:**

1. **Specialized single-bit reader:**
```haskell
readBit :: BitReader -> (Bool, BitReader)
readBit (BitReader bytes offset bits count)
  | count >= 1 = (bits .&. 1 == 1, BitReader bytes offset (bits `shiftR` 1) (count - 1))
  | otherwise = let (b, r) = readBits 1 reader in (b /= 0, r)
```

2. **Batch refill:** Pre-refill to 64 bits at decode start instead of per-read.

3. **Word-level reads:** Use direct 8-byte reads for refill instead of byte-by-byte.

**Impact:** 8-12% speedup on VP8L decoding.

---

### 4. Loop Filter Interior Fast Path

**File:** `src/Codec/Picture/WebP/Internal/VP8/LoopFilter.hs`

**Issue:** The loop filter has scattered memory access patterns:
- Nested `forM_` with stride arithmetic creates poor cache locality
- Bounds checking on every pixel read/write (even interior pixels)
- Limit values recalculated per filter call

**Improvements:**

1. **Interior pixel fast path:** Skip bounds checks for pixels known to be in-bounds:
```haskell
filterVEdgeInterior :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Int -> Int -> ST s ()
-- Direct array indexing without bounds checks
```

2. **Vectorize horizontal filters:** H-filters process contiguous pixels - load 4-8 at once.

3. **Pre-compute limits:** Cache `mbEdgeLimit`, `subEdgeLimit`, `hevThresh` once per frame.

**Impact:** 5-8% speedup on VP8 lossy decoding.

---

### 5. Transform Fusion

**File:** `src/Codec/Picture/WebP/Internal/VP8L/Transform.hs`

**Issue:** Per-pixel transforms without vectorization:
- Extract R,G,B, modify, repack for every pixel
- Inverse color transform does 4 lookups + 3 multiplications per pixel
- Predictor transform reads 4 neighbors per pixel (not cached)

**Improvements:**

1. **Fuse subtract green + color transform:** Single pass instead of two:
```haskell
forM_ [0 .. totalPixels - 1] $ \i -> do
  pixel <- VSM.read pixels i
  -- Apply both transforms atomically
```

2. **Cache predictor neighbors:** Process in tiles, keep top row in registers.

3. **Lazy sign extension:** Defer `toInt8` conversion until needed.

**Impact:** 12-18% speedup on VP8L with transforms.

---

### 6. Transform Neighbor Caching

**File:** `src/Codec/Picture/WebP/Internal/VP8L/Transform.hs:125-166`

**Issue:** The predictor transform reads 4-5 neighbors per pixel with bounds checks:

```haskell
-- Current (lines 138-161):
pixel <- VSM.read pixels idx

left <- if x > 0 then VSM.read pixels (idx - 1) else return 0xFF000000
top <-
  if y > 0
    then do
      let topIdxInteger = (fromIntegral (y - 1) :: Integer) * ...
          topIdx = fromIntegral topIdxInteger :: Int
      VSM.read pixels topIdx
    else return 0xFF000000
topLeft <-
  if x > 0 && y > 0
    then do
      let tlIdxInteger = (fromIntegral (y - 1) :: Integer) * ...
      VSM.read pixels tlIdx
    else return 0xFF000000
topRight <-
  if x < width - 1 && y > 0
    then do
      let trIdxInteger = ...
      VSM.read pixels trIdx
    else return top
```

Each pixel does:
- 4-5 conditional branches
- 4-5 `VSM.read` calls
- Integer arithmetic for index calculation (slow)

**Solution:** Process row-by-row with cached previous row:

```haskell
inversePredictorTransform sizeBits transformData width height pixels = do
  -- Pre-allocate row buffers
  prevRow <- VSM.replicate width 0xFF000000

  forM_ [0 .. height - 1] $ \y -> do
    let rowStart = y * width
    left <- newSTRef 0xFF000000

    forM_ [0 .. width - 1] $ \x -> do
      let idx = rowStart + x
      pixel <- VSM.read pixels idx

      -- Read from cached row (no Integer arithmetic)
      top <- VSM.read prevRow x
      topLeft <- if x > 0 then VSM.read prevRow (x - 1) else return 0xFF000000
      topRight <- if x < width - 1 then VSM.read prevRow (x + 1) else return top
      l <- readSTRef left

      let predicted = predictor mode l top topLeft topRight
          result = addPixels pixel predicted

      VSM.write pixels idx result
      writeSTRef left result
      VSM.write prevRow x result  -- Update for next row
```

**Impact:** 8-12% speedup on predictor transform heavy images.

---

### 7. Integer → Int Arithmetic

**Files:**
- `src/Codec/Picture/WebP/Internal/VP8L/LZ77.hs:303-313`
- `src/Codec/Picture/WebP/Internal/VP8L/Transform.hs:80-81, 88, 130, 135, 144, 151, 158`

**Issue:** Code uses `Integer` for index calculations to avoid overflow:

```haskell
-- LZ77.hs lines 303-312:
let -- Use Integer arithmetic to avoid overflow
    baseInteger :: Integer
    baseInteger =
      if distCode < 4
        then fromIntegral distCode + 1
        else
          let shifted = (fromIntegral ((distCode - 2) .&. complement 1)) `shiftL` extraBits2
           in 5 + shifted
    distInteger = baseInteger + fromIntegral extra2 + 1
    dist' = fromIntegral distInteger :: Int

-- Transform.hs lines 80-81:
transformIdxInteger = (fromIntegral transformY :: Integer) * (fromIntegral transformWidth :: Integer) + ...
transformIdx = fromIntegral transformIdxInteger :: Int
```

`Integer` operations are 5-10x slower than `Int` due to arbitrary precision.

**Solution:** Use `Int64` or `Word64` for overflow-safe arithmetic:

```haskell
-- Use Int64 for intermediate calculations
let transformIdx64 = fromIntegral transformY * fromIntegral transformWidth + fromIntegral transformX :: Int64
    transformIdx = fromIntegral transformIdx64 :: Int
```

For distance calculations in LZ77, the values are bounded by image dimensions (max 16384), so `Int` with explicit bounds checks is safe:

```haskell
-- distCode is 0-39, extraBits2 is 0-20, extra2 fits in extraBits2 bits
-- Maximum distance is bounded by total pixels (16384^2), fits in Int
let base = if distCode < 4
             then distCode + 1
             else 5 + ((distCode - 2) .&. complement 1) `shiftL` extraBits2
    dist = base + fromIntegral extra2 + 1
```

**Impact:** 5-10% speedup in transform and LZ77 decode loops.

---

## MEDIUM Priority

### 8. Prefix Code Specialization

**File:** `src/Codec/Picture/WebP/Internal/VP8L/PrefixCode.hs`

**Issue:** Two-level lookup on every symbol:
- 2 table lookups for codes > 8 bits
- Generic bounds checks on vector indexing

**Improvements:**

1. **Single-level codes:** For alphabets with max code length ≤ 8 bits, use flat 256-entry table.

2. **Inline table lookups:** Add `{-# INLINE #-}` pragmas on hot paths.

3. **Cache table entry:** When decoding runs of same-length symbols, reuse entry.

**Impact:** 6-10% speedup on VP8L decode.

---

### 9. LZ77 Copy Unrolling

**File:** `src/Codec/Picture/WebP/Internal/VP8L/LZ77.hs`

**Issue:** Byte-by-byte copy loop:
- 127-byte run = 127 separate ST operations
- Color cache updated on every copy
- No special case for `dist=1` (repeat previous pixel)

**Improvements:**

1. **Special case dist=1:**
```haskell
if dist == 1
  then replicatePixel ...  -- Fill with single value
  else copyLoopGeneral ...
```

2. **Unroll copy loop:** Copy 4 pixels at once (16 bytes).

3. **Sample cache updates:** Insert every Nth pixel instead of every pixel.

**Impact:** 8-15% speedup on LZ77-heavy images.

---

### 10. Mode Selection Batching

**File:** `src/Codec/Picture/WebP/Internal/VP8/ModeSelection.hs`

**Issue:** SAD calculated separately for all 4 modes:
- Buffer cloned 4 times per macroblock
- No early termination if good mode found
- Nested loops without striping

**Improvements:**

1. **Incremental SAD:** Compute SAD inline while generating prediction.

2. **Early exit:** Stop when SAD < threshold.

3. **Manual vectorization:** Use `Word16` pairs to process 2 pixels per operation.

**Impact:** 15-25% speedup on VP8 lossy encoding.

---

### 11. Huffman Sort Optimization

**File:** `src/Codec/Picture/WebP/Internal/VP8L/EncodeComplete.hs:196, 218`

**Issue:** Huffman code generation converts vectors to lists for sorting:

```haskell
-- Line 196:
sortedSymFreqs = VU.fromList $ sortBy (comparing snd) $ VU.toList symFreqs

-- Line 218:
sortedDesc = VU.fromList $ reverse $ sortBy (comparing snd) $ VU.toList symFreqs
```

This creates multiple intermediate lists: `toList` → `sortBy` → `reverse` → `fromList`.

**Solution:** Use vector sorting algorithms directly:

```haskell
import qualified Data.Vector.Algorithms.Intro as VA

-- Sort in place
sortedSymFreqs <- do
  mv <- VU.thaw symFreqs
  VA.sortBy (comparing snd) mv
  VU.unsafeFreeze mv

-- For descending, use Down wrapper
sortedDesc <- do
  mv <- VU.thaw symFreqs
  VA.sortBy (comparing (Down . snd)) mv
  VU.unsafeFreeze mv
```

**Impact:** 5-10% speedup on VP8L encoding (per-frame Huffman generation).

---

### 12. Predictor Mode Early Exit

**File:** `src/Codec/Picture/WebP/Internal/VP8L/PredictorEncode.hs:89-90`

**Issue:** Mode selection evaluates all 14 modes before picking best:

```haskell
-- Lines 89-90:
sads = map (computeBlockSAD startX startY endX endY width height pixels) [0 .. 13]
(bestMode, _) = foldl1 (\a@(_, sa) b@(_, sb) -> if sb < sa then b else a) (zip [0 ..] sads)
```

Problems:
1. Lazy list `[0..13]` with lazy `map` — thunks build up
2. `foldl1` is not strict — accumulator not forced
3. Always evaluates all 14 modes even if an early mode is perfect (SAD = 0)

**Solution:** Use strict fold with early exit:

```haskell
selectBestMode sizeBits bx by width height pixels =
  let blockSize = 1 `shiftL` sizeBits
      -- ... setup ...

      -- Strict early-exit search
      go !bestMode !bestSAD !mode
        | mode > 13 = bestMode
        | bestSAD == 0 = bestMode  -- Can't improve on 0
        | otherwise =
            let sad = computeBlockSAD startX startY endX endY width height pixels mode
             in if sad < bestSAD
                  then go mode sad (mode + 1)
                  else go bestMode bestSAD (mode + 1)
   in go 0 maxBound 0
```

Also, compute SADs incrementally while generating predictions:

```haskell
computeBlockSAD ... mode = runST $ do
  sadRef <- newSTRef 0
  forM_ [(x, y) | y <- [startY..endY-1], x <- [startX..endX-1]] $ \(x, y) -> do
    sad <- pixelSAD width height pixels mode (x, y)
    modifySTRef' sadRef (+ sad)
  readSTRef sadRef
```

**Impact:** 10-15% speedup on predictor transform encoding (common case exits early).

---

## LOW Priority

### 13. IDCT/DCT Optimization

**Files:** `src/Codec/Picture/WebP/Internal/VP8/IDCT.hs`, `DCT.hs`

**Issue:** 8 separate read/write calls per 4x4 block.

**Improvements:**

1. **Load-operate-store batching:** Load all 16 coefficients, operate, store back.

2. **Inline constants:** Pre-compute common products.

**Impact:** 3-5% for VP8 lossy.

---

### 14. Huffman Table Construction

**File:** `src/Codec/Picture/WebP/Internal/VP8L/PrefixCode.hs`

**Issue:** Many small mutations during table build (66 operations across nested loops).

**Improvements:**

1. **Pre-allocate work arrays:** Single allocation for all temporary arrays.

2. **Flatten loops:** Use work queue instead of 3-level nesting.

**Impact:** 8-12% speedup on lossless encode.

---

### 15. INLINE Pragmas

**Files:** Multiple hot-path functions

**Issue:** Critical inner-loop functions lack `{-# INLINE #-}` pragmas:

```haskell
-- Transform.hs - predictor functions called millions of times
predictor :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
addPixels :: Word32 -> Word32 -> Word32
subPixels :: Word32 -> Word32 -> Word32
avgPixels2 :: Word32 -> Word32 -> Word32

-- LZ77.hs - called per pixel
packColor :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
colorCacheHash :: Word32 -> Int -> Int

-- PredictorEncode.hs - duplicated predictor functions
predictor :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
```

Without `INLINE`, GHC may not inline these across module boundaries, causing function call overhead on every pixel.

**Solution:** Add pragmas to all hot-path functions:

```haskell
{-# INLINE predictor #-}
predictor :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
predictor 0 _ _ _ _ = 0xFF000000
...

{-# INLINE addPixels #-}
addPixels :: Word32 -> Word32 -> Word32

{-# INLINE packColor #-}
packColor :: Word8 -> Word8 -> Word8 -> Word8 -> Word32

{-# INLINE colorCacheHash #-}
colorCacheHash :: Word32 -> Int -> Int
```

Also consider `INLINABLE` for polymorphic functions to enable specialization.

**Impact:** 2-5% speedup across all decode/encode paths.

---

### 16. Strictness Annotations

**Files:** Multiple

**Issue:** Some intermediate bindings lack bang patterns, causing thunk buildup:

```haskell
-- Transform.hs line 98-99:
let tmpRed = (fromIntegral r + colorTransformDelta greenToRed (toInt8 g)) .&. 0xFF
    tmpBlue = ...  -- Not forced until used

-- LZ77.hs line 254:
let color = packColor ...  -- Lazy binding
```

**Solution:** Add bang patterns to force evaluation:

```haskell
let !tmpRed = (fromIntegral r + colorTransformDelta greenToRed (toInt8 g)) .&. 0xFF
    !tmpBlue = ...

let !color = packColor ...
```

Or use `$!` for strict application in loops.

**Impact:** 1-3% speedup (reduces GC pressure).

---

## Code Consolidation Opportunities

### Duplicate Predictor Functions

**Files:**
- `src/Codec/Picture/WebP/Internal/VP8L/Transform.hs:169-186` (14 predictor modes)
- `src/Codec/Picture/WebP/Internal/VP8L/PredictorEncode.hs:132-148` (same 14 modes)

Both files have identical implementations of:
- `predictor` (14 modes)
- `addPixels`, `subPixels`
- `avgPixels2`, `avgPixels3`
- `clampAddSubtractFull`, `clampAddSubtractHalf`

**Solution:** Extract to a shared `VP8L.Predictor` module:

```haskell
-- src/Codec/Picture/WebP/Internal/VP8L/Predictor.hs
module Codec.Picture.WebP.Internal.VP8L.Predictor
  ( predictor
  , addPixels, subPixels
  , avgPixels2, avgPixels3
  , clampAddSubtractFull, clampAddSubtractHalf
  ) where

{-# INLINE predictor #-}
predictor :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
...
```

Benefits:
- Single source of truth
- Easier to add INLINE pragmas once
- Reduces binary size

---

## Currently Applied Optimizations

The following optimizations are already implemented:

- **Mutable vectors in ST monad** for all pixel buffers
- **64-bit bit reader buffer** with `unsafeIndex` for hot paths
- **Two-level Huffman lookup tables** for O(1) symbol decode
- **INLINE pragmas** on critical functions
- **Strict record fields** for minimal allocations
- **Loop filter** enabled based on quality (quality < 100)

---

## Benchmarking Notes

To measure impact of changes:

```bash
# Build with profiling
stack build --profile

# Run with profiling
stack exec -- your-test-program +RTS -p -s

# Or use criterion for micro-benchmarks
stack bench
```

Key metrics to track:
- Decode time per megapixel
- Encode time per megapixel
- Memory allocations (GC pressure)
- Cache miss rate (with perf/cachegrind)
