# Performance Improvement Opportunities

This document describes actionable performance improvements for the JuicyPixels-webp codec. Improvements are prioritized by impact and implementation effort.

## Summary

| Improvement | Files | Est. Impact | Effort | Priority |
|-------------|-------|-------------|--------|----------|
| BitReader fast paths | BitReader.hs | 8-12% | Low | HIGH |
| Loop filter interior fast path | LoopFilter.hs | 5-8% | Medium | HIGH |
| Transform fusion | Transform.hs | 12-18% | Medium | HIGH |
| Prefix code specialization | PrefixCode.hs | 6-10% | Medium | MEDIUM |
| LZ77 copy unrolling | LZ77.hs | 8-15% | Low | MEDIUM |
| Mode selection batching | ModeSelection.hs | 15-25% | Medium | MEDIUM |
| IDCT/DCT optimization | IDCT.hs, DCT.hs | 3-5% | Low | LOW |
| Huffman table build | PrefixCode.hs | 8-12% | High | LOW |

**Overall potential gains:**
- VP8L decoding: 35-55% faster
- VP8 lossy decoding: 15-25% faster
- VP8L encoding: 20-35% faster
- VP8 lossy encoding: 15-20% faster

---

## HIGH Priority

### 1. BitReader Fast Paths

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

### 2. Loop Filter Interior Fast Path

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

### 3. Transform Fusion

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

## MEDIUM Priority

### 4. Prefix Code Specialization

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

### 5. LZ77 Copy Unrolling

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

### 6. Mode Selection Batching

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

## LOW Priority

### 7. IDCT/DCT Optimization

**Files:** `src/Codec/Picture/WebP/Internal/VP8/IDCT.hs`, `DCT.hs`

**Issue:** 8 separate read/write calls per 4x4 block.

**Improvements:**

1. **Load-operate-store batching:** Load all 16 coefficients, operate, store back.

2. **Inline constants:** Pre-compute common products.

**Impact:** 3-5% for VP8 lossy.

---

### 8. Huffman Table Construction

**File:** `src/Codec/Picture/WebP/Internal/VP8L/PrefixCode.hs`

**Issue:** Many small mutations during table build (66 operations across nested loops).

**Improvements:**

1. **Pre-allocate work arrays:** Single allocation for all temporary arrays.

2. **Flatten loops:** Use work queue instead of 3-level nesting.

**Impact:** 8-12% speedup on lossless encode.

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
