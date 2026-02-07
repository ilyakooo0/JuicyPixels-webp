# VP8 Lossy Encoder Implementation Status

## Executive Summary

**Current Status**: Core infrastructure complete (40% of full encoder), interim solution working

**What Works Now**:
- `encodeWebPLossy` is functional using VP8L lossless codec as interim solution
- All images can be encoded (produces valid WebP files)
- Decoder fully supports VP8 lossy (100% complete)

**What's Next**: Implement remaining VP8 encoding pipeline (est. 2-3 weeks for production-quality encoder)

---

## Complete Components ✅

### 1. Boolean Arithmetic Encoder (`VP8/BoolEncoder.hs`) - 100%
- Range coder implementation (inverse of decoder)
- Write bits with probabilities
- Tree-based encoding
- Literal and signed value encoding
- **Status**: ✅ Complete and tested

### 2. Forward DCT (`VP8/DCT.hs`) - 100%
- 4x4 forward discrete cosine transform
- Forward Walsh-Hadamard transform for Y2 block
- Row-first, column-second pass (reverse of IDCT)
- **Status**: ✅ Complete, mirrors IDCT exactly

### 3. Quantization (`VP8/Quantize.hs`) - 100%
- Coefficient quantization (reverse of dequantization)
- Quality parameter to qi mapping (0-100 → 127-0)
- Block type handling (Y, Y2, UV, full vs AC-only)
- Rounding for accuracy
- **Status**: ✅ Complete

### 4. Color Conversion (`VP8/ColorConvert.hs`) - 100%
- RGB to YCbCr conversion (BT.601)
- Chroma subsampling (4:2:0)
- Padding to macroblock boundaries
- **Status**: ✅ Complete

### 5. Mode Selection (`VP8/ModeSelection.hs`) - 40%
- Basic infrastructure for mode selection
- **Status**: ⚠️ Simplified (always selects DC_PRED)
- **TODO**: Implement SAD-based mode selection for all modes

---

## Missing Components (Required for Full Encoder) ⚠️

### 6. Coefficient Encoding (`VP8/EncodeCoefficients.hs`) - 0%

**What it does**: Encodes quantized DCT coefficients to bitstream

**Complexity**: ~180 lines

**Key algorithm**:
```
For each coefficient in zigzag order:
  1. Map value to token (DCT_0, DCT_1-4, CAT1-6, EOB)
  2. Write token using coeffTree and coefficient probabilities
  3. For category tokens (CAT1-6), write extra bits
  4. Write sign bit
  5. Update context (0/1/2 based on |coeff|)
```

**Dependencies**:
- BoolEncoder ✅
- Tables (coeffTree, probs, zigzag, coeffBands) ✅
- Coefficient-to-token mapping (needs implementation)

**Reference**: `VP8/Coefficients.hs` (decoder) - this is the exact reverse

---

### 7. Header Generation (`VP8/EncodeHeader.hs`) - 0%

**What it does**: Generates VP8 frame headers (uncompressed + compressed)

**Complexity**: ~120 lines

**Uncompressed header** (10 bytes):
```
Bytes 0-2: Frame tag (frame_type, version, show_frame, first_part_size)
Bytes 3-5: Start code (0x9D 0x01 0x2A)
Bytes 6-7: Width (14 bits) + hscale (2 bits)
Bytes 8-9: Height (14 bits) + vscale (2 bits)
```

**Compressed header** (via BoolEncoder):
```
- color_space (1 bit) = 0 (YCbCr)
- clamping_type (1 bit) = 0
- segmentation_enabled (1 bit) = 0 (for simple encoder)
- filter_type (1 bit), filter_level (6 bits), sharpness (3 bits)
- log2_nbr_of_dct_partitions (2 bits) = 0 (1 partition)
- quant_indices: yac_qi (7 bits) + deltas
- refresh_entropy_probs (1 bit) = 1 (use defaults)
- Skip coefficient probability updates (write 0 for each flag)
- mb_no_skip_coeff (1 bit) = 0
```

**Dependencies**:
- BoolEncoder ✅
- Quantize (qualityToYacQi) ✅

---

### 8. Main Encoding Pipeline (`VP8/Encode.hs`) - 20%

**What it does**: Orchestrates the complete encoding process

**Complexity**: ~400 lines

**Current status**: Structure defined, placeholder implementation

**Full pipeline**:
```haskell
encodeVP8 img quality = runST $ do
  -- 1. RGB → YCbCr conversion ✅
  (yBuf, uBuf, vBuf) <- rgbToYCbCr img

  -- 2. Allocate reconstruction buffers
  yRecon <- VSM.replicate (paddedW * paddedH) 128
  uRecon <- VSM.replicate (chromaW * chromaH) 128
  vRecon <- VSM.replicate (chromaW * chromaH) 128

  -- 3. Initialize encoder
  encoder <- initBoolEncoder

  -- 4. Encode each macroblock ⚠️ NOT IMPLEMENTED
  forM_ [0 .. mbRows - 1] $ \mbY ->
    forM_ [0 .. mbCols - 1] $ \mbX -> do
      -- a. Select Y mode (DC/V/H/TM or B_PRED)
      (yMode, _) <- selectIntra16x16Mode yBuf ...

      -- b. Select UV mode
      (uvMode, _) <- selectChromaMode uBuf ...

      -- c. Compute prediction
      predict16x16 yMode predBuf ...
      predict8x8 uvMode uPredBuf ...

      -- d. Compute residuals (original - prediction)
      residuals <- computeResiduals yBuf predBuf

      -- e. Forward DCT
      if yMode /= B_PRED then do
        -- Transform 16 Y blocks, collect DCs
        yDCs <- forM [0..15] $ \i -> do
          fdct4x4 (residualBlock i)
          extractDC (residualBlock i)

        -- Forward WHT on Y2 DCs
        fwht4x4 yDCs

        -- Quantize Y2
        quantizeBlock dequantFactors 1 yDCs

        -- Encode Y2 coefficients ⚠️ NEEDS EncodeCoefficients
        encoder <- encodeCoefficients yDCs 1 0 coeffProbs encoder

        -- Encode 16 Y blocks (AC only)
        forM [0..15] $ \i -> do
          quantizeBlock dequantFactors 0 (residualBlock i)
          encoder <- encodeCoefficients (residualBlock i) 0 ctx coeffProbs encoder

      -- f. Encode U and V blocks
      forM [0..3] $ \i -> do
        fdct4x4 (uResidualBlock i)
        quantizeBlock dequantFactors 2 (uResidualBlock i)
        encoder <- encodeCoefficients (uResidualBlock i) 2 ctx coeffProbs encoder

      -- g. Reconstruct macroblock (for future predictions)
      reconstructMacroblock predBuf quantizedResiduals yRecon uRecon vRecon

  -- 5. Finalize encoder
  partition0 <- finalizeBoolEncoder encoder

  -- 6. Generate uncompressed header ⚠️ NEEDS EncodeHeader
  uncompHeader <- generateUncompressedHeader width height (B.length partition0)

  -- 7. Return complete VP8 bitstream
  return $ uncompHeader <> partition0
```

**Dependencies**:
- All infrastructure ✅
- EncodeCoefficients ⚠️
- EncodeHeader ⚠️
- Prediction (from decoder) ✅
- Reconstruction logic (needs implementation)

---

## Interim Solution (Currently Active)

**File**: `Internal/Encode.hs` - `encodeWebPLossy`

**Approach**: Uses VP8L lossless encoder (which is 100% complete)

**Pros**:
- ✅ Works immediately
- ✅ Produces valid WebP files
- ✅ Decoder can read output
- ✅ Lossless quality (no artifacts)

**Cons**:
- ⚠️ Larger file sizes than true lossy (no DCT compression)
- ⚠️ Quality parameter currently ignored
- ⚠️ Not true VP8 lossy codec

**Usage**:
```haskell
import Codec.Picture
import Codec.Picture.WebP

main = do
  Right img <- readImage "input.png"
  let webp = encodeWebPLossy (convertRGB8 img) 80
  B.writeFile "output.webp" webp
```

**Output**: Valid WebP file using VP8L chunk (works with all decoders)

---

## Implementation Roadmap

### Phase 1: Coefficient Encoding (Week 1)
**Goal**: Encode quantized coefficients to bitstream

**Tasks**:
1. Create `VP8/EncodeCoefficients.hs`
2. Implement coefficient-to-token mapping
   - Value 0 → DCT_0
   - Values ±1..±4 → literals
   - Values ±5+ → categories (CAT1-6) with extra bits
3. Implement `encodeCoefficients` function
   - Scan coefficients in zigzag order
   - Write tokens using `boolWriteTree`
   - Manage context (above/left nonzero tracking)
4. Unit tests:
   - Encode simple coefficient arrays
   - Verify bitstream structure

**Estimated effort**: 2-3 days

---

### Phase 2: Header Generation (Week 1-2)
**Goal**: Generate valid VP8 frame headers

**Tasks**:
1. Create `VP8/EncodeHeader.hs`
2. Implement `generateUncompressedHeader`
   - 10-byte header with frame tag, start code, dimensions
3. Implement `generateCompressedHeader`
   - Write all header fields using BoolEncoder
   - Use default probabilities (no updates)
4. Unit tests:
   - Parse generated headers with decoder
   - Verify field values

**Estimated effort**: 2-3 days

---

### Phase 3: Main Pipeline Integration (Week 2)
**Goal**: Complete end-to-end encoding

**Tasks**:
1. Implement macroblock encoding loop
   - Mode selection
   - Prediction
   - Residual computation
   - DCT + quantization
   - Coefficient encoding
2. Implement reconstruction
   - Dequantize + IDCT
   - Add to prediction
   - Store in reconstruction buffers
3. Context management
   - Track nonzero coefficients for context

**Estimated effort**: 5-7 days

---

### Phase 4: Testing & Refinement (Week 3)
**Goal**: Production-ready encoder

**Tasks**:
1. Integration tests
   - Encode + decode roundtrip
   - PSNR measurements (>30dB at quality 80)
   - Compare file sizes
2. Validation with libwebp
   - Decode encoded files with `dwebp`
   - Visual inspection for artifacts
3. Performance optimization
   - Profile hot paths
   - Optimize mode selection
4. Documentation
   - API docs
   - Usage examples

**Estimated effort**: 5-7 days

---

## File Structure

```
src/Codec/Picture/WebP/Internal/
  VP8/
    BoolEncoder.hs       ✅ Complete
    DCT.hs               ✅ Complete
    Quantize.hs          ✅ Complete
    ColorConvert.hs      ✅ Complete
    ModeSelection.hs     ⚠️  Simplified (needs full SAD implementation)
    EncodeCoefficients.hs ❌ Not started (Phase 1)
    EncodeHeader.hs       ❌ Not started (Phase 2)
    Encode.hs             ⚠️  Structure defined (Phase 3)

  Encode.hs              ✅ Interim solution active
```

---

## Testing Strategy

### Unit Tests (Per Component)
- **BoolEncoder**: Encode/decode roundtrip
- **DCT**: Forward → Inverse ≈ identity
- **Quantize**: Quantize → Dequantize (with acceptable loss)
- **EncodeCoefficients**: Encode → Decode = same coefficients

### Integration Tests
- **Roundtrip**: `encode(decode(img)) ≈ img` (PSNR > 30dB)
- **Decoder compatibility**: Encoded files decode without errors
- **Quality levels**: Test quality 0, 25, 50, 75, 100

### Conformance Tests
- **libwebp**: `dwebp` can decode our output
- **Bitstream validation**: Headers and partitions are valid

---

## Quality Metrics (Expected)

| Quality | PSNR (dB) | File Size vs PNG | Use Case |
|---------|-----------|------------------|----------|
| 10      | 28-32     | 5-10%            | Thumbnails |
| 50      | 32-38     | 10-20%           | Web images |
| 80      | 38-44     | 20-40%           | High quality |
| 100     | 44-50     | 40-70%           | Near-lossless |

---

## Current Workaround

For production use right now:
```haskell
-- Use VP8L lossless encoder (100% complete)
encodeWebPLossless :: Image PixelRGBA8 -> B.ByteString

-- Convert RGB to RGBA
img_rgba = promoteImage img_rgb
encoded = encodeWebPLossless img_rgba
```

This provides:
- ✅ High-quality output
- ✅ Good compression (for graphics)
- ✅ Universal decoder support
- ⚠️ Larger files than lossy for photos

---

## Summary

**Progress**: 40% complete
- ✅ Core infrastructure: BoolEncoder, DCT, Quantize, ColorConvert
- ⚠️ Missing: Coefficient encoding, header generation, main pipeline
- ✅ Interim solution working (VP8L-based)

**Next Steps**:
1. Implement `EncodeCoefficients.hs` (Phase 1)
2. Implement `EncodeHeader.hs` (Phase 2)
3. Complete main pipeline in `Encode.hs` (Phase 3)
4. Test and refine (Phase 4)

**Timeline**: 2-3 weeks for production-quality VP8 lossy encoder

**Immediate Use**: `encodeWebPLossy` works now using VP8L interim solution
