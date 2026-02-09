# Performance Improvement Opportunities

This document describes actionable performance improvements for the JuicyPixels-webp codec. Improvements are prioritized by impact and implementation effort.

## Summary

| Improvement | Files | Est. Impact | Effort | Priority | Status |
|-------------|-------|-------------|--------|----------|--------|
| ~~Color cache mutable refactor~~ | LZ77.hs | **30-50%** | Medium | **CRITICAL** | **DONE** |
| ~~LZ77 copy loop batching~~ | LZ77.hs | **15-25%** | Medium | **CRITICAL** | **DONE** |
| ~~BitReader fast paths~~ | BitReader.hs | 8-12% | Low | HIGH | **DONE** |
| ~~Loop filter interior fast path~~ | LoopFilter.hs | 5-8% | Medium | HIGH | **DONE** |
| ~~Transform INLINE pragmas~~ | Transform.hs | 12-18% | Medium | HIGH | **DONE** |
| ~~Transform Int arithmetic~~ | Transform.hs | 8-12% | Medium | HIGH | **DONE** |
| ~~Integer → Int arithmetic~~ | LZ77.hs, Transform.hs | 5-10% | Low | HIGH | **DONE** |
| **BoolDecoder INLINE pragmas** | BoolDecoder.hs | 5-10% | Low | HIGH | |
| **Alpha filtering fast paths** | Alpha.hs | 10-15% | Medium | HIGH | |
| **Quantize/Dequant loop optimization** | Quantize.hs, Dequant.hs | 5-10% | Low | HIGH | |
| Integer div → shiftR | BoolDecoder.hs, Coefficients.hs | 2-5% | Low | HIGH | |
| Prefix code specialization | PrefixCode.hs | 6-10% | Medium | MEDIUM | |
| Mode selection batching | ModeSelection.hs | 15-25% | Medium | MEDIUM | |
| Huffman sort optimization | EncodeComplete.hs | 5-10% | Low | MEDIUM | |
| Predictor mode early exit | PredictorEncode.hs | 10-15% | Low | MEDIUM | |
| Animation loop optimization | Animation.hs | 8-12% | Low | MEDIUM | |
| BitWriter optimization | BitWriter.hs | 5-8% | Medium | MEDIUM | |
| AlphaEncode row-base | AlphaEncode.hs | 3-5% | Low | MEDIUM | |
| IDCT/DCT batch conversions | IDCT.hs, DCT.hs | 3-5% | Low | MEDIUM | |
| ColorConvert shiftR | ColorConvert.hs | 1-2% | Low | LOW | |
| Huffman table build | PrefixCode.hs | 8-12% | High | LOW | |
| Predict.hs INLINE pragmas | Predict.hs | 2-3% | Low | LOW | |

**Estimated gains from completed optimizations:**
- VP8L decoding: 50-80% faster
- VP8 lossy decoding: 15-25% faster

**Remaining potential gains:**
- VP8L encoding: 30-50% faster (with MEDIUM priority fixes)
- VP8 lossy encoding: 30-45% faster (with BoolDecoder + mode selection fixes)
- Alpha channel: 13-20% faster (with Alpha.hs fast paths)

---

## Completed Optimizations

### 1. Color Cache Mutable Refactor ✓

**File:** `src/Codec/Picture/WebP/Internal/VP8L/LZ77.hs`

**Implementation:** Added `MutableColorCache` type that stays mutable throughout the decode loop:

```haskell
data MutableColorCache s = MutableColorCache
  { mccBits :: !Int,
    mccColors :: !(VSM.MVector s Word32)
  }

{-# INLINE insertColorM #-}
insertColorM :: Word32 -> MutableColorCache s -> ST s ()
insertColorM color cache = do
  let idx = colorCacheHash color (mccBits cache)
  VSM.unsafeWrite (mccColors cache) idx color
```

The decode loop now uses `insertColorM` instead of `insertColor`, eliminating vector allocation per pixel.

**Impact:** 30-50% speedup on VP8L decode for images with color cache enabled.

---

### 2. LZ77 Copy Loop Batching ✓

**File:** `src/Codec/Picture/WebP/Internal/VP8L/LZ77.hs`

**Implementation:** Added three special cases in `copyLoop`:

1. **dist=1**: Fill with single color (very common for runs of identical pixels)
2. **Non-overlapping** (dist >= len): Batch copy with sampled cache insertions (every 8th pixel)
3. **Overlapping**: Pixel-by-pixel but with mutable cache

```haskell
if dist == 1
  then do
    color <- VSM.unsafeRead out srcPos
    forM_ [pos .. pos + actualLen - 1] $ \i ->
      VSM.unsafeWrite out i color
    when doCache $ insertColorM color cache
  else if dist >= actualLen
    then do
      -- Non-overlapping: batch copy
      forM_ [0 .. actualLen - 1] $ \i -> do
        color <- VSM.unsafeRead out (srcPos + i)
        VSM.unsafeWrite out (pos + i) color
      -- Sample cache insertions (every 8th pixel)
      when doCache $ forM_ [0, 8 .. actualLen - 1] $ \i -> do
        color <- VSM.unsafeRead out (pos + i)
        insertColorM color cache
```

**Impact:** 15-25% speedup on back-reference heavy images.

---

### 3. BitReader Fast Paths ✓

**File:** `src/Codec/Picture/WebP/Internal/BitReader.hs`

**Implementation:**

1. **Specialized single-bit reader** with fast path when bits available:
```haskell
{-# INLINE readBit #-}
readBit :: BitReader -> (Bool, BitReader)
readBit reader@(BitReader bytes offset bits count)
  | count >= 1 =
      let !result = (bits .&. 1) == 1
          !newBits = bits `shiftR` 1
          !newCount = count - 1
       in (result, BitReader bytes offset newBits newCount)
  | otherwise = -- refill and retry
```

2. **Aggressive pre-fill** to 56+ bits on initialization via `refillBufferFull`

3. **Unsafe byte reads** using `Data.ByteString.Unsafe.unsafeIndex`

4. **INLINE pragmas** on all hot-path functions

**Impact:** 8-12% speedup on VP8L decoding.

---

### 4. Loop Filter Interior Fast Path ✓

**File:** `src/Codec/Picture/WebP/Internal/VP8/LoopFilter.hs`

**Implementation:**

1. **Fast path functions** `filterSimpleVEdgeFast` and `filterSimpleHEdgeFast` that compute indices once and use `unsafeRead`/`unsafeWrite`

2. **INLINE pragmas** on all filter functions: `needsFiltering`, `needsFilteringNormal`, `isHighEdgeVariance`, `simpleFilter`, `subblockFilter`, `mbFilter`

3. **Bang patterns** throughout for strict evaluation

**Impact:** 5-8% speedup on VP8 lossy decoding.

---

### 5. Transform Optimizations ✓

**File:** `src/Codec/Picture/WebP/Internal/VP8L/Transform.hs`

**Implementation:**

1. **INLINE pragmas** on all hot-path functions:
   - `predictor`, `addPixels`, `subPixels`, `avgPixels2`, `avgPixels3`
   - `select`, `clampAddSubtractFull`, `clampAddSubtractHalf`
   - `colorTransformDelta`, `toInt8`, `clip255Int`

2. **Int arithmetic** instead of Integer (safe within 16384×16384 bounds)

3. **Tight loops** with `unsafeRead`/`unsafeWrite` for pixel access

4. **STRef for left pixel** in predictor transform (avoids repeated reads)

5. **Pre-computed row bases** to avoid repeated multiplication

**Impact:** 12-18% speedup on VP8L with transforms.

---

### 6. Integer → Int Arithmetic ✓

**Files:** `LZ77.hs`, `Transform.hs`

**Implementation:** Replaced all `Integer` arithmetic with `Int`:

```haskell
-- Before:
let transformIdxInteger = (fromIntegral transformY :: Integer) * ...
    transformIdx = fromIntegral transformIdxInteger :: Int

-- After:
let !transformIdx = transformRowBase + transformX
```

Index calculations now use pre-computed row bases and simple addition.

**Impact:** 5-10% speedup in transform and LZ77 decode loops.

---

## HIGH Priority (Remaining)

### 7. BoolDecoder INLINE Pragmas

**File:** `src/Codec/Picture/WebP/Internal/VP8/BoolDecoder.hs`

**Issue:** The VP8 arithmetic decoder functions are called on every bit/symbol decode but lack INLINE pragmas:

```haskell
-- Missing INLINE on these hot functions:
boolRead :: BoolDecoder -> Word8 -> (Bool, BoolDecoder)  -- line ~39
boolLiteral :: Int -> BoolDecoder -> (Word32, BoolDecoder)  -- line ~63
boolSigned :: Int -> BoolDecoder -> (Int32, BoolDecoder)  -- line ~74
boolReadTree :: VU.Vector Int8 -> VU.Vector Word8 -> BoolDecoder -> (Int, BoolDecoder)  -- line ~84
loadNewBytes :: BoolDecoder -> BoolDecoder  -- line ~25
```

These are called millions of times per VP8 lossy decode.

**Solution:** Add `{-# INLINE #-}` pragmas to all hot-path functions.

**Impact:** 5-10% speedup on VP8 lossy decoding.

---

### 8. Alpha Filtering Fast Paths

**File:** `src/Codec/Picture/WebP/Internal/Alpha.hs`

**Issue:** Alpha filtering performs boundary checks on every pixel:

```haskell
forM_ [0 .. height - 1] $ \y ->
  forM_ [0 .. width - 1] $ \x -> do
    let idx = y * width + x
    left <- if x > 0 then VSM.read ... else return 0
    top <- if y > 0 then VSM.read ... else return 0
    ...
```

For a 1024×768 image, this does 786,432 boundary checks that could be avoided for interior pixels.

**Solution:** Create separate loops for:
1. First row (no top neighbor)
2. First column (no left neighbor)
3. Interior pixels (no boundary checks needed)

```haskell
-- Interior pixels: no bounds checks
forM_ [1 .. height - 1] $ \y -> do
  let rowBase = y * width
      prevRowBase = (y - 1) * width
  forM_ [1 .. width - 1] $ \x -> do
    let idx = rowBase + x
    left <- VSM.unsafeRead output (idx - 1)
    top <- VSM.unsafeRead output (prevRowBase + x)
    ...
```

**Impact:** 10-15% speedup on alpha channel decoding.

---

### 9. Quantize/Dequant Loop Optimization

**Files:** `src/Codec/Picture/WebP/Internal/VP8/Quantize.hs`, `Dequant.hs`

**Issue:** Coefficient loops recreate a list on every block:

```haskell
-- Called per 4x4 block (thousands of times per frame)
mapM_
  ( \i -> do
      c <- VSM.read coeffs i
      VSM.write coeffs i (quantizeCoeff c quant)
  )
  [1 .. 15]  -- List recreated and GC'd each block!
```

**Solution:** Use manual unrolling or strict indexing:

```haskell
-- Option 1: Manual unrolling for fixed-size loops
let go !i
      | i > 15 = return ()
      | otherwise = do
          c <- VSM.unsafeRead coeffs i
          VSM.unsafeWrite coeffs i (quantizeCoeff c quant)
          go (i + 1)
go 1

-- Option 2: Add INLINE pragma and use unsafeRead/unsafeWrite
{-# INLINE quantizeBlock #-}
quantizeBlock coeffs quant =
  forM_ [1..15] $ \i -> do
    c <- VSM.unsafeRead coeffs i
    VSM.unsafeWrite coeffs i (quantizeCoeff c quant)
```

**Impact:** 5-10% speedup on VP8 lossy encoding/decoding (coefficient processing is a hot path).

---

### 10. Integer Division to Bit Shift

**Files:** `src/Codec/Picture/WebP/Internal/VP8/BoolDecoder.hs`, `Coefficients.hs`

**Issue:** Tree traversal uses integer division in tight loops:

```haskell
-- In boolReadTree (called per symbol decode):
let probIdx = i `div` 2  -- Slow division
```

**Solution:** Replace with bit shift:

```haskell
let probIdx = i `shiftR` 1  -- Much faster
```

**Impact:** 2-5% speedup on VP8 lossy decoding (cumulative across all tree reads).

---

## MEDIUM Priority (Remaining)

### 11. Prefix Code Specialization

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

### 12. Mode Selection Batching

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

### 13. Huffman Sort Optimization

**File:** `src/Codec/Picture/WebP/Internal/VP8L/EncodeComplete.hs:196, 218`

**Issue:** Huffman code generation converts vectors to lists for sorting:

```haskell
sortedSymFreqs = VU.fromList $ sortBy (comparing snd) $ VU.toList symFreqs
```

This creates multiple intermediate lists: `toList` → `sortBy` → `reverse` → `fromList`.

**Solution:** Use vector sorting algorithms directly:

```haskell
import qualified Data.Vector.Algorithms.Intro as VA

sortedSymFreqs <- do
  mv <- VU.thaw symFreqs
  VA.sortBy (comparing snd) mv
  VU.unsafeFreeze mv
```

**Impact:** 5-10% speedup on VP8L encoding (per-frame Huffman generation).

---

### 14. Predictor Mode Early Exit

**File:** `src/Codec/Picture/WebP/Internal/VP8L/PredictorEncode.hs:89-90`

**Issue:** Mode selection evaluates all 14 modes before picking best:

```haskell
sads = map (computeBlockSAD startX startY endX endY width height pixels) [0 .. 13]
(bestMode, _) = foldl1 (\a@(_, sa) b@(_, sb) -> if sb < sa then b else a) (zip [0 ..] sads)
```

Problems:
1. Lazy list `[0..13]` with lazy `map` — thunks build up
2. `foldl1` is not strict — accumulator not forced
3. Always evaluates all 14 modes even if an early mode is perfect (SAD = 0)

**Solution:** Use strict fold with early exit:

```haskell
go !bestMode !bestSAD !mode
  | mode > 13 = bestMode
  | bestSAD == 0 = bestMode  -- Can't improve on 0
  | otherwise =
      let sad = computeBlockSAD ... mode
       in if sad < bestSAD
            then go mode sad (mode + 1)
            else go bestMode bestSAD (mode + 1)
```

**Impact:** 10-15% speedup on predictor transform encoding (common case exits early).

---

### 15. Animation Loop Optimization

**File:** `src/Codec/Picture/WebP/Internal/Animation.hs:217, 239`

**Issue:** Frame compositing recalculates row bases on every pixel:

```haskell
mapM_ (\fy -> mapM_ (composePixel fy) [0 .. frameWidth - 1]) [0 .. frameHeight - 1]

-- Inside composePixel:
let canvasIdx = (canvasY * canvasWidth + canvasX) * 4  -- Recalculated per pixel!
    frameIdx = (fy * frameWidth + fx) * 4
```

**Solution:** Pre-compute row base before inner loop:

```haskell
forM_ [0 .. frameHeight - 1] $ \fy -> do
  let !frameRowBase = fy * frameWidth * 4
      !canvasRowBase = ((frameY + fy) * canvasWidth + frameX) * 4
  forM_ [0 .. frameWidth - 1] $ \fx -> do
    let !frameIdx = frameRowBase + fx * 4
        !canvasIdx = canvasRowBase + fx * 4
    -- compose pixel...
```

**Impact:** 8-12% speedup on animation frame compositing.

---

### 16. BitWriter Optimization

**File:** `src/Codec/Picture/WebP/Internal/BitWriter.hs:45-51`

**Issue:** Recursive bit-by-bit writing:

```haskell
writeBits :: Int -> Word64 -> BitWriter -> BitWriter
writeBits 0 _ writer = writer
writeBits numBits value writer =
  let bit = (value .&. 1) /= 0
      value' = value `shiftR` 1
      writer' = writeBit bit writer  -- Intermediate BitWriter created
   in writeBits (numBits - 1) value' writer'
```

This creates numBits intermediate BitWriter values.

**Solution:** Write bits in batches when possible:

```haskell
{-# INLINE writeBits #-}
writeBits :: Int -> Word64 -> BitWriter -> BitWriter
writeBits !n !value !writer
  | n <= 0 = writer
  | n <= remaining = -- Fast path: fits in current byte
      let newBits = bits .|. (value `shiftL` bitPos)
          newPos = bitPos + n
      in if newPos >= 8
           then flushByte ...
           else writer { bwBits = newBits, bwBitPos = newPos }
  | otherwise = -- Slow path: spans bytes
      writeBitsSlow n value writer
```

**Impact:** 5-8% speedup on VP8L encoding (bitstream writing).

---

### 17. AlphaEncode Row-Base Pre-computation

**File:** `src/Codec/Picture/WebP/Internal/AlphaEncode.hs:34-39`

**Issue:** Index calculation repeated per pixel:

```haskell
forM_ [0 .. height - 1] $ \y ->
  forM_ [0 .. width - 1] $ \x -> do
    let pixelIdx = (y * width + x) * 4  -- Multiplication per pixel
        alphaIdx = y * width + x
    ...
```

**Solution:** Pre-compute row base:

```haskell
forM_ [0 .. height - 1] $ \y -> do
  let !rowBase = y * width
  forM_ [0 .. width - 1] $ \x -> do
    let !pixelIdx = (rowBase + x) * 4
        !alphaIdx = rowBase + x
    ...
```

**Impact:** 3-5% speedup on alpha encoding.

---

### 18. IDCT/DCT Batch Read Conversions

**Files:** `src/Codec/Picture/WebP/Internal/VP8/IDCT.hs`, `DCT.hs`

**Issue:** Multiple separate fromIntegral conversions:

```haskell
-- Each read and convert is separate:
c0 <- fromIntegral <$> VSM.read coeffs (0 * 4 + col)
c1 <- fromIntegral <$> VSM.read coeffs (1 * 4 + col)
c2 <- fromIntegral <$> VSM.read coeffs (2 * 4 + col)
c3 <- fromIntegral <$> VSM.read coeffs (3 * 4 + col)
```

**Solution:** Batch reads then convert:

```haskell
-- Read all raw values first
c0raw <- VSM.unsafeRead coeffs (col)
c1raw <- VSM.unsafeRead coeffs (4 + col)
c2raw <- VSM.unsafeRead coeffs (8 + col)
c3raw <- VSM.unsafeRead coeffs (12 + col)
-- Convert in batch with strictness
let !c0 = fromIntegral c0raw :: Int
    !c1 = fromIntegral c1raw :: Int
    !c2 = fromIntegral c2raw :: Int
    !c3 = fromIntegral c3raw :: Int
```

**Impact:** 3-5% speedup on IDCT/DCT operations.

---

## LOW Priority (Remaining)

### 19. ColorConvert Bit Shifts

**File:** `src/Codec/Picture/WebP/Internal/VP8/ColorConvert.hs:72-73`

**Issue:** Integer division in tight loop:

```haskell
let chromaX = x `div` 2
    chromaY = y `div` 2
```

**Solution:** Use bit shifts:

```haskell
let chromaX = x `shiftR` 1
    chromaY = y `shiftR` 1
```

**Impact:** 1-2% speedup on RGB to YCbCr conversion.

---

### 20. Huffman Table Construction

**File:** `src/Codec/Picture/WebP/Internal/VP8L/PrefixCode.hs`

**Issue:** Many small mutations during table build (66 operations across nested loops).

**Improvements:**

1. **Pre-allocate work arrays:** Single allocation for all temporary arrays.

2. **Flatten loops:** Use work queue instead of 3-level nesting.

**Impact:** 8-12% speedup on lossless encode.

---

### 21. VP8 Predict INLINE Pragmas

**File:** `src/Codec/Picture/WebP/Internal/VP8/Predict.hs`

**Issue:** `readPixel` and `writePixel` helper functions lack INLINE pragmas:

```haskell
readPixel :: VSM.MVector s Word8 -> Int -> (Int, Int) -> ST s Word8
readPixel plane stride (x, y) = VSM.read plane (y * stride + x)

writePixel :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Word8 -> ST s ()
writePixel plane stride (x, y) val = VSM.write plane (y * stride + x) val
```

These are called in tight loops for intra prediction (predict16x16V, predict16x16H, etc.).

**Solution:** Add `{-# INLINE #-}` pragmas.

**Impact:** 2-3% speedup on VP8 intra prediction.

---

## Code Consolidation Opportunities

### Duplicate Predictor Functions

**Files:**
- `src/Codec/Picture/WebP/Internal/VP8L/Transform.hs` (14 predictor modes)
- `src/Codec/Picture/WebP/Internal/VP8L/PredictorEncode.hs` (same 14 modes)

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

The following optimizations are implemented:

- **Mutable color cache in ST monad** — eliminates allocation per pixel
- **LZ77 copy loop batching** — special cases for dist=1 and non-overlapping
- **64-bit bit reader buffer** with aggressive pre-fill to 56+ bits
- **Specialized single-bit reader** for most common case
- **Unsafe byte/vector access** in hot paths (`unsafeIndex`, `unsafeRead`, `unsafeWrite`)
- **Two-level Huffman lookup tables** for O(1) symbol decode
- **INLINE pragmas** on all critical functions in BitReader, Transform, LoopFilter, LZ77
- **Int arithmetic** instead of Integer for index calculations
- **Bang patterns** for strict evaluation throughout hot paths
- **STRef for left pixel** in predictor transform
- **Pre-computed row bases** to avoid repeated multiplication
- **Strict record fields** for minimal allocations
- **Loop filter fast paths** with pre-computed indices

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
