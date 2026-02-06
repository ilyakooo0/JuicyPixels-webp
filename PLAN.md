# Implementation Plan: JuicyPixels-webp

Pure Haskell WebP image decoder integrating with JuicyPixels types. Decode-only (no encoding). Covers lossy (VP8), lossless (VP8L), alpha, and animation.

## Module Structure

```
Codec/Picture/
  WebP.hs                              -- Public API
  WebP/
    Internal/
      Container.hs                     -- RIFF parsing, chunk dispatch
      BitReader.hs                     -- LSB-first bit reader (VP8L + ALPH)
      VP8L.hs                          -- VP8L lossless decoder top-level
      VP8L/
        PrefixCode.hs                  -- Canonical Huffman build + decode
        Transform.hs                   -- Predictor, color, subtract-green, color-indexing
        LZ77.hs                        -- LZ77 decode loop (literals, backrefs, cache)
      VP8.hs                           -- VP8 lossy decoder top-level
      VP8/
        BoolDecoder.hs                 -- Boolean arithmetic coder (range decoder)
        Header.hs                      -- Compressed frame header parsing
        Predict.hs                     -- Intra prediction (16x16, 8x8, 4x4 modes)
        Coefficients.hs                -- Token tree decode, context management
        Dequant.hs                     -- Dequantization + lookup tables
        IDCT.hs                        -- 4x4 IDCT and Walsh-Hadamard transform
        LoopFilter.hs                  -- Normal and simple loop filters
        Tables.hs                      -- All constant tables (probs, quant, trees)
      Alpha.hs                         -- ALPH chunk decoder
      Animation.hs                     -- ANIM/ANMF frame assembly
```

## Public API

```haskell
module Codec.Picture.WebP
  ( decodeWebP
  , decodeWebPWithMetadata
  , decodeWebPFirstFrame        -- first frame only for animated images
  , decodeWebPAnimation         -- all frames with durations
  , WebPAnimFrame(..)           -- frame + duration + position
  ) where

decodeWebP :: ByteString -> Either String DynamicImage
decodeWebPWithMetadata :: ByteString -> Either String (DynamicImage, Metadatas)
decodeWebPFirstFrame :: ByteString -> Either String DynamicImage
decodeWebPAnimation :: ByteString -> Either String [WebPAnimFrame]

data WebPAnimFrame = WebPAnimFrame
  { webpFrameImage    :: DynamicImage
  , webpFrameDuration :: Int          -- milliseconds
  , webpFrameX        :: Int
  , webpFrameY        :: Int
  }
```

Output types:
- VP8L lossless with alpha -> `ImageRGBA8`
- VP8L lossless without alpha -> `ImageRGB8` (drop alpha channel)
- VP8 lossy without alpha -> `ImageRGB8` (YCbCr->RGB, `ImageYCbCr8` is also an option but RGB is more useful)
- VP8 lossy + ALPH -> `ImageRGBA8` (RGB from VP8 + alpha from ALPH)

Metadata: Use `mkSizeMetadata` for Width/Height. Cannot add `SourceWebP` to the closed `SourceFormat` type from JuicyPixels, so omit `Format` key. Include EXIF/XMP as raw `ByteString` metadata if the chunks are present.

## Dependencies

Current: `base`, `bytestring`, `JuicyPixels`

Additional needed:
- `vector` -- Storable vectors for pixel buffers, mutable vectors for decode state (already a transitive dep of JuicyPixels)
- `primitive` -- PrimMonad for mutable vector operations (transitive dep of vector)
- `binary` -- `Data.Binary.Get` for RIFF chunk parsing (already a dep of JuicyPixels)

No other dependencies needed. All algorithms (arithmetic coding, Huffman, IDCT, LZ77, loop filter) are implemented from scratch.

## Implementation Phases

### Phase 1: Foundation (Container + BitReader)

**Goal**: Parse any WebP file into identified chunks, and provide the VP8L bit-reading primitive.

#### 1a. Container Parser (`Container.hs`)

Parse the RIFF envelope and dispatch:

```haskell
data WebPFile
  = WebPSimpleLossy   VP8Chunk
  | WebPSimpleLossless VP8LChunk
  | WebPExtended       VP8XHeader [WebPChunk]

data VP8XHeader = VP8XHeader
  { vp8xHasICC       :: Bool
  , vp8xHasAlpha     :: Bool
  , vp8xHasExif      :: Bool
  , vp8xHasXMP       :: Bool
  , vp8xHasAnimation :: Bool
  , vp8xCanvasWidth  :: Int     -- 1-based (field + 1)
  , vp8xCanvasHeight :: Int     -- 1-based (field + 1)
  }

data WebPChunk
  = ChunkVP8  ByteString         -- lossy bitstream
  | ChunkVP8L ByteString         -- lossless bitstream
  | ChunkALPH ByteString         -- alpha channel
  | ChunkANIM AnimHeader
  | ChunkANMF AnimFrame [WebPChunk]  -- frame with nested chunks
  | ChunkICCP ByteString
  | ChunkEXIF ByteString
  | ChunkXMP  ByteString
  | ChunkUnknown FourCC ByteString
```

Implementation with `Data.Binary.Get`:
1. Read 12-byte RIFF header, verify "RIFF" + "WEBP"
2. Peek at first chunk FourCC to determine variant
3. Parse chunks sequentially. Each chunk: 4-byte FourCC + 4-byte LE size + payload + optional pad byte
4. For ANMF chunks, recursively parse nested sub-chunks
5. Validate: reserved fields are read but not rejected (per spec)

#### 1b. BitReader (`BitReader.hs`)

VP8L reads bits LSB-first from bytes. This is the hot path -- every Huffman decode and every ReadBits call goes through it.

```haskell
data BitReader = BitReader
  { brBytes  :: !ByteString
  , brOffset :: !Int           -- byte offset
  , brBits   :: !Word64        -- bit buffer (pre-fetched)
  , brCount  :: !Int           -- valid bits in buffer
  }

initBitReader :: ByteString -> BitReader
readBits :: Int -> BitReader -> (Word32, BitReader)
readBit  :: BitReader -> (Bool, BitReader)
```

Keep a 64-bit buffer, refill when `brCount < 32`. Extract `n` bits from LSB: `result = brBits .&. ((1 `shiftL` n) - 1)`, then `brBits >>= n`, `brCount -= n`.

Performance note: Use `unsafeIndex` for the byte buffer. The bit reader will be called millions of times per image. Consider using an `ST` monad with `STRef` for the mutable state, or pass the reader as a strict record in a `State` monad.

#### 1c. Tests for Phase 1

- Parse known WebP files (lossy, lossless, extended) and verify chunk structure
- BitReader: verify `readBits(2)` from byte `0b_11001010` yields `10` (bits 0-1), then `00` (bits 2-3), etc.
- Round-trip: write a BitReader test that reads known bit sequences

### Phase 2: VP8L Lossless Decoder

**Goal**: Decode simple lossless WebP images (the `VP8L` chunk) to `Image PixelRGBA8`.

This is the most self-contained codec and a good starting point.

#### 2a. Prefix Code Construction (`PrefixCode.hs`)

```haskell
data PrefixCode
  = PrefixCodeSingle !Word16                    -- single symbol (0-bit code)
  | PrefixCodeTable !(Vector Word32) !Int       -- lookup table + bits

buildPrefixCode :: Vector Int -> Either String PrefixCode

-- Decode one symbol from the bitstream
decodeSymbol :: PrefixCode -> BitReader -> (Word16, BitReader)
```

Two-level table approach for decoding speed:
- Primary table: 8-bit lookup (256 entries). Each entry stores `(symbol, length)` packed into Word32.
- Secondary tables: for codes > 8 bits (up to 15). Primary entry stores offset into secondary table.
- This gives O(1) decode for codes <= 8 bits, O(1) for longer codes with one extra table lookup.

Construction algorithm (from docs/webp-format.md):
1. Count code lengths -> `bl_count[length]`
2. Compute `next_code` array per the canonical Huffman algorithm
3. Assign codes, build the lookup table

Simple code length code (1 or 2 symbols):
- 1 symbol: `PrefixCodeSingle symbol` (no bits consumed on decode)
- 2 symbols: both get code length 1, build a 1-bit table

Normal code length code (two-level scheme):
1. Read `num_code_lengths = ReadBits(4) + 4`
2. Read 3-bit code lengths in `kCodeLengthCodeOrder` = `[17,18,0,1,2,3,4,5,16,6,7,8,9,10,11,12,13,14,15]`
3. Build prefix code from these lengths (alphabet size = 19)
4. Use that code to read the actual code lengths, handling repeat codes 16 (repeat previous nonzero or 8), 17 (zeros 3..10), 18 (zeros 11..138)
5. Build prefix code from the decoded lengths

#### 2b. VP8L Image Decode (`VP8L.hs`, `LZ77.hs`)

Top-level flow:

```haskell
decodeVP8L :: ByteString -> Either String (Image PixelRGBA8)
```

1. **Read VP8L header**: signature byte 0x2F, 14-bit width, 14-bit height, alpha_is_used, version (must be 0)

2. **Read transforms** (while `readBit` returns 1):
   - Accumulate transforms in order (max 4, each type at most once)
   - Predictor/Color transforms: read `size_bits`, then decode a subresolution image (recursion!)
   - Color indexing: read table size, decode color table image, apply subtraction decoding
   - Subtract green: no data

3. **Decode spatially-coded-image**:
   a. Read color cache info (optional, 1..11 bits)
   b. Read meta prefix codes (optional): decode entropy subresolution image, compute number of prefix code groups
   c. For each prefix code group, read 5 prefix codes (green+len+cache, R, B, A, distance)
   d. Run the LZ77 decode loop (in `LZ77.hs`)

4. **Apply inverse transforms** in reverse order

LZ77 decode loop (`LZ77.hs`):

```haskell
decodeLZ77 :: VP8LDecodeState -> BitReader -> (MVector s Word32, BitReader)
```

For each pixel position:
- Read symbol S from prefix code #1
- S < 256: literal pixel (read R, B, A from codes #2-4), insert into color cache
- 256 <= S < 280: LZ77 backref. Decode length (prefix + extra bits), decode distance (code #5 + extra bits + distance map for codes 1-120). Copy L pixels one at a time (allowing overlap). Insert each into cache.
- S >= 280: Color cache lookup at `S - 280`. Insert result into cache.

Key data structures:
```haskell
data VP8LDecodeState = VP8LDecodeState
  { vp8lWidth        :: !Int
  , vp8lHeight       :: !Int
  , vp8lColorCache   :: !(Maybe ColorCache)
  , vp8lPrefixGroups :: !(Vector PrefixCodeGroup)
  , vp8lEntropyImage :: !(Maybe (Vector Word32, Int))  -- pixels + prefix_bits
  }

data ColorCache = ColorCache
  { ccBits   :: !Int
  , ccColors :: !(MVector s Word32)    -- size = 2^ccBits
  }
```

The distance map table (120 entries, from docs/webp-format.md) needs to be a constant array.

#### 2c. VP8L Transforms (`Transform.hs`)

```haskell
data VP8LTransform
  = TransformPredictor  !Int !(Vector Word32)       -- size_bits, subres image
  | TransformColor      !Int !(Vector Word32)       -- size_bits, subres image
  | TransformSubGreen
  | TransformColorIndex !(Vector Word32) !Int       -- palette, width_bits

applyInverseTransforms :: [VP8LTransform] -> Int -> Int -> MVector s Word32 -> ST s ()
```

Apply in reverse order:

**Inverse subtract green**: For each pixel, `red = (red + green) .&. 0xff`, `blue = (blue + green) .&. 0xff`.

**Inverse color transform**: For each block (determined by size_bits), extract `ColorTransformElement` from subresolution image. Apply:
```
tmp_red  = (red  + colorTransformDelta(green_to_red, green)) .&. 0xff
tmp_blue = (blue + colorTransformDelta(green_to_blue, green)
                 + colorTransformDelta(red_to_blue, tmp_red)) .&. 0xff
```
where `colorTransformDelta(t, c) = (toInt8(t) * toInt8(c)) >> 5`. **Critical**: uses the already-modified `tmp_red` for `red_to_blue`.

**Inverse predictor**: 14 modes (0-13). For each pixel, look up mode from subresolution image (green channel). Compute predicted ARGB from neighbors, add to residual (mod 256 per component). Special cases for row 0 and column 0 borders.

**Inverse color indexing**: Look up each pixel in the palette using the green channel as index. If width_bits > 0, unbundle packed pixels (multiple indices per green byte, LSB-first).

#### 2d. Subresolution Image Decoding

Subresolution images (used by predictor, color, and entropy image transforms) are decoded as `entropy-coded-image`:
- They have **NO transforms** (not even the 0-bit transform-present terminator)
- They have **NO meta prefix codes** (always a single prefix code group)
- They DO have optional color cache
- Their dimensions come from the transform: `ceil(width / 2^size_bits)` x `ceil(height / 2^size_bits)`

This means the VP8L decode is recursive: decoding the main image requires first decoding 0-3 subresolution images. Each subresolution image decode is a simpler version of the same process (just color-cache + prefix codes + LZ77, no transforms, no meta prefix codes).

#### 2e. Tests for Phase 2

- Decode known lossless WebP images and compare pixel-exact output against reference (e.g., `dwebp -pam` output)
- Test each transform in isolation with crafted inputs
- Test prefix code construction with known code-length sequences
- Test LZ77 overlap copy (distance < length)
- Test color cache hash function against known values

### Phase 3: VP8 Lossy Decoder

**Goal**: Decode lossy WebP images (the `VP8 ` chunk) to `Image PixelRGB8`.

This is the most complex part. Implement bottom-up: primitives first, then compose.

#### 3a. Boolean Arithmetic Decoder (`BoolDecoder.hs`)

```haskell
data BoolDecoder = BoolDecoder
  { bdBytes :: !ByteString
  , bdRange :: !Word32          -- [128..255]
  , bdValue :: !Word32
  , bdCount :: !Int             -- bits consumed from current byte
  , bdPos   :: !Int             -- byte position
  }

initBoolDecoder :: ByteString -> BoolDecoder
boolRead     :: Word8 -> BoolDecoder -> (Bool, BoolDecoder)      -- read bit with probability
boolLiteral  :: Int -> BoolDecoder -> (Word32, BoolDecoder)      -- n bits, prob=128, MSB-first
boolSigned   :: Int -> BoolDecoder -> (Int32, BoolDecoder)       -- literal + sign
boolReadTree :: Vector Int8 -> Vector Word8 -> BoolDecoder -> (Int, BoolDecoder)
```

The `boolRead` implementation:
1. `split = 1 + ((range - 1) * fromIntegral prob) `shiftR` 8`
2. `bigsplit = split `shiftL` 8` (or appropriate shift depending on value representation)
3. Compare `value` against `bigsplit`
4. Update `range` and `value` accordingly
5. Renormalize: shift left while `range < 128`, every 8 shifts read new byte

This is the most critical primitive. Every single field in the VP8 compressed data is read through this. Must be bit-exact.

**Performance**: Consider using `ST` with `STRef` for the mutable state, or unboxed strict fields in a state-passing style. The decoder is called millions of times per image.

#### 3b. Constant Tables (`Tables.hs`)

All the large constant tables from `docs/vp8-bitstream.md`:

```haskell
-- Trees (Int8 vectors, negative = leaf token, positive = child node index)
kfYmodeTree    :: Vector Int8     -- 8 elements
kfBmodeTree    :: Vector Int8     -- 18 elements
kfUvModeTree   :: Vector Int8     -- 6 elements
mbSegmentTree  :: Vector Int8     -- 6 elements
coeffTree      :: Vector Int8     -- 22 elements (12 leaf tokens)

-- Fixed probabilities
kfYmodeProb    :: Vector Word8    -- {145, 156, 163, 128}
kfUvModeProb   :: Vector Word8    -- {142, 114, 183}

-- Context-dependent probabilities (900 values)
kfBmodeProbs   :: Vector Word8    -- 10*10*9 = 900 values, index: above*90 + left*9 + i

-- Coefficient probabilities (1056 values each)
defaultCoeffProbs :: Vector Word8 -- 4*8*3*11
coeffUpdateProbs  :: Vector Word8 -- 4*8*3*11

-- Quantization lookup tables (128 entries each)
dcQLookup :: Vector Word16        -- index 0..127
acQLookup :: Vector Word16        -- index 0..127

-- Other constants
zigzag     :: Vector Int          -- 16 entries: {0,1,4,8,5,2,3,6,9,12,13,10,7,11,14,15}
coeffBands :: Vector Int          -- 16 entries: {0,1,2,3,6,4,5,6,6,6,6,6,6,6,6,7}

-- Category extra-bit probabilities
pcatProbs :: Vector (Vector Word8)  -- Pcat1..Pcat6

-- Context index mappings
leftContextIndex  :: Vector Int   -- 25 entries
aboveContextIndex :: Vector Int   -- 25 entries
```

These are transcribed from the reference doc (already verified against RFC 6386).

#### 3c. Frame Header Parsing (`Header.hs`)

Parse the uncompressed header (10 bytes) and the compressed header (via BoolDecoder).

```haskell
data VP8FrameHeader = VP8FrameHeader
  { vp8Width          :: !Int
  , vp8Height         :: !Int
  , vp8HScale         :: !Int
  , vp8VScale         :: !Int
  , vp8ColorSpace     :: !Int        -- 0=YCbCr
  , vp8ClampingReq    :: !Bool
  , vp8Segments       :: !(Maybe SegmentInfo)
  , vp8FilterType     :: !Int        -- 0=normal, 1=simple
  , vp8FilterLevel    :: !Int        -- 0-63
  , vp8Sharpness      :: !Int        -- 0-7
  , vp8FilterDeltas   :: !(Maybe FilterDeltas)
  , vp8NumDCTPartitions :: !Int      -- 1, 2, 4, or 8
  , vp8QuantIndices   :: !QuantIndices
  , vp8CoeffProbs     :: !(Vector Word8)  -- 4*8*3*11 = 1056, after updates
  , vp8SkipEnabled    :: !Bool
  , vp8ProbSkipFalse  :: !Word8
  }
```

Compressed header field order (critical to get right):
1. `color_space` (1 bit)
2. `clamping_type` (1 bit)
3. `segmentation_enabled` (1 bit) + segment data
4. `filter_type` (1 bit), `filter_level` (6 bits), `sharpness_level` (3 bits) + filter deltas
5. `log2_nbr_of_dct_partitions` L(2) -- **comes BEFORE quant_indices**
6. `quant_indices`: yac_qi (7 bits) + 5 optional deltas
7. `refresh_entropy_probs` (1 bit)
8. Coefficient probability updates: 4-deep nested loop, gated by `coeffUpdateProbs`
9. `mb_no_skip_coeff` (1 bit) + `prob_skip_false` (8 bits)

After parsing, also read the DCT partition sizes: `(N-1)` 3-byte LE values after the first partition.

#### 3d. Coefficient Decoding (`Coefficients.hs`)

```haskell
decodeCoefficients
  :: BoolDecoder              -- DCT partition decoder
  -> VP8FrameHeader
  -> Int                      -- block type (0-3)
  -> Int                      -- initial context (0, 1, or 2)
  -> Int                      -- start position (0 or 1)
  -> (Vector Int16, Bool, BoolDecoder)  -- coefficients, has_nonzero, updated decoder
```

Implementation:
1. Start at the given coefficient position (0 for most blocks, 1 for Y blocks in non-B_PRED mode since DC comes from WHT)
2. For each position, determine band from `coeffBands[pos]`
3. Read token from `coeffTree` using `boolReadTree` with probabilities from `coeffProbs[type][band][ctx]`
4. If EOB: done, remaining coefficients are 0
5. If DCT_0: coefficient = 0, ctx = 0 for next. **Important**: after DCT_0, skip EOB branch on next token (start tree at index 2)
6. If literal (1-4): coefficient = literal value
7. If category: read extra bits using category probability tables, compute value
8. Read sign bit (prob=128)
9. Place at `zigzag[pos]`
10. Update context: 0 if coeff==0, 1 if |coeff|==1, 2 if |coeff|>=2

Context management:
- Maintain `above_nz :: MVector s Word8` (9 entries per MB column) and `left_nz :: MVector s Word8` (9 entries)
- Initial context for subblock `i` = `above_nz[aboveContextIndex[i]] + left_nz[leftContextIndex[i]]`
- After decoding a subblock, update both context arrays with whether any nonzero coefficient was found

#### 3e. Intra Prediction (`Predict.hs`)

```haskell
-- 16x16 luma prediction (writes 256 pixels)
predict16x16 :: Int -> MVector s Word8 -> Int -> Int -> Int -> ST s ()

-- 8x8 chroma prediction (writes 64 pixels)
predict8x8 :: Int -> MVector s Word8 -> Int -> Int -> Int -> ST s ()

-- 4x4 sub-block prediction (writes 16 pixels)
predict4x4 :: Int -> MVector s Word8 -> Int -> Int -> Int -> ST s ()
```

For each mode, implement the exact pixel formulas from docs/vp8-bitstream.md:
- **DC_PRED**: Average of above row + left column, with special cases for top-left/top-row/left-column macroblocks
- **V_PRED**: Copy the above row
- **H_PRED**: Copy the left column
- **TM_PRED**: `clip255(above[x] + left[y] - above_left)`
- **B_PRED sub-modes** (10 modes): Use `avg2` and `avg3` helper functions with specific pixel patterns per mode. Need above (A-H), left (I-L), and above-left (M) reference pixels. Edge pixels come from the reconstructed frame buffer (previously decoded macroblocks/subblocks).

The B_PRED modes require careful handling of which reference pixels to use, especially for subblocks at macroblock boundaries (pixels come from the adjacent macroblock's reconstruction).

#### 3f. Dequantization (`Dequant.hs`)

```haskell
data DequantFactors = DequantFactors
  { dqYDC  :: !Int16
  , dqYAC  :: !Int16
  , dqY2DC :: !Int16
  , dqY2AC :: !Int16
  , dqUVDC :: !Int16
  , dqUVAC :: !Int16
  }

computeDequantFactors :: QuantIndices -> Maybe SegmentInfo -> Vector DequantFactors
dequantize :: DequantFactors -> Int -> MVector s Int16 -> ST s ()
```

For each segment (1-4), compute 6 quantizer values from the base `yac_qi` plus deltas. Apply lookup tables `dcQLookup` and `acQLookup`, with special post-processing:
- `y2dc *= 2`
- `y2ac = y2ac * 155 / 100` (minimum 8)
- `uvdc` capped at 132

Dequantization: multiply each coefficient by its factor (`dc_quant` for position 0, `ac_quant` for positions 1-15).

#### 3g. Inverse Transforms (`IDCT.hs`)

```haskell
idct4x4   :: MVector s Int16 -> ST s ()       -- in-place 4x4 IDCT
iWHT4x4   :: MVector s Int16 -> ST s (Vector Int16)  -- WHT producing 16 DC values
```

**IDCT** (columns-first, then rows):
- Uses constants `cospi8sqrt2minus1 = 20091`, `sinpi8sqrt2 = 35468`
- Column pass: no rounding bias
- Row pass: add 4, shift right by 3 (`(x + 4) >> 3`)
- All arithmetic is exact integer (Int32 intermediate, Int16 output)

**WHT** (for the Y2 DC block):
- Same column-first, row-second structure
- Butterfly: `a = in0 + in3, b = in1 + in2, c = in1 - in2, d = in0 - in3`
- Output: `[a+b, c+d, a-b, d-c]`
- Column pass: add 3, shift right by 3

After WHT, distribute the 16 output values as the DC coefficient (`coeffs[0]`) of each of the 16 Y subblocks.

#### 3h. Loop Filter (`LoopFilter.hs`)

```haskell
applyLoopFilter :: VP8FrameHeader -> MVector s Word8 -> Int -> Int -> ST s ()
```

Two filter types:

**Simple filter** (Y plane only):
- At each edge (MB and subblock), test `|p0 - q0| * 2 + |p1 - q1| >> 1 <= limit`
- Apply: `filter = clip(3*(q0-p0) + clip(p1-q1))`, `Filter1 = clip(filter+4) >> 3`, `Filter2 = clip(filter+3) >> 3`
- `p0 += Filter2`, `q0 -= Filter1`

**Normal filter** (Y, U, V planes):
- Filter limit computation: `interior_limit` from level and sharpness, `mb_edge_limit = (level+2)*2 + interior_limit`, `sub_edge_limit = level*2 + interior_limit`
- `hev_threshold`: 0 if level < 15, 1 if 15 <= level < 40, 2 if level >= 40

Two sub-functions:
1. `subblockFilter` (at subblock edges): HEV -> modify p0/q0 only; non-HEV -> modify p0/q0/p1/q1 using `(Filter1+1)>>1`
2. `mbFilter` (at MB edges): HEV -> same as subblock; non-HEV -> 6-pixel 27/18/9 weighted filter

Application order: MB vertical edges, subblock vertical edges, MB horizontal edges, subblock horizontal edges. Each direction processes pixels perpendicular to the edge.

**Key gotchas** (from MEMORY.md):
- Clamp-then-shift: `clip(w+4) >> 3`, NOT `clip((w+4)>>3)`
- Non-HEV P1/Q1 uses `(Filter1 + 1) >> 1`, not `(raw_w + 1) >> 1`

#### 3i. VP8 Top-Level Decode (`VP8.hs`)

Tie everything together:

```haskell
decodeVP8 :: ByteString -> Either String (Image PixelRGB8)
```

1. Parse uncompressed header (10 bytes)
2. Initialize BoolDecoder for first partition
3. Parse compressed header (all fields in order)
4. Initialize DCT partition decoders (1/2/4/8)
5. Allocate reconstruction buffers: Y (`mbRows*16 x mbCols*16`), U (`mbRows*8 x mbCols*8`), V (same)
6. For each macroblock row:
   - Select DCT partition: `row % numDCTPartitions`
   - Reset left context array
   - For each macroblock column:
     a. Read segment ID (if segmentation enabled)
     b. Read skip flag (if skip enabled)
     c. Read Y mode. If B_PRED, read 16 sub-block modes.
     d. Read UV mode
     e. If not skip:
        - For non-B_PRED: decode Y2 block (type 1), WHT, distribute DCs to Y subblocks, decode 16 Y blocks (type 0, start at position 1)
        - For B_PRED: decode 16 Y blocks (type 3, start at position 0)
        - Decode 4 U blocks and 4 V blocks (type 2)
     f. Dequantize all blocks
     g. Apply prediction (writes base pixels)
     h. Add IDCT residuals to prediction
     i. Clip all pixels to [0, 255]
7. Apply loop filter to the entire frame
8. Convert YCbCr to RGB (with chroma upsampling)
9. Crop to declared dimensions (frame is padded to MB boundary)

#### 3j. Tests for Phase 3

- BoolDecoder: test with known byte sequences, verify bit-exact output
- Coefficient decoding: craft a minimal 1-macroblock VP8 frame, verify coefficients
- IDCT: test with known input/output pairs (e.g., all-zero input = all-zero output, DC-only input)
- Prediction: test each mode against reference pixel patterns
- Full decode: compare against `dwebp -pam` output for a set of lossy WebP images
- Loop filter: compare filtered output against reference implementation

### Phase 4: Alpha Channel

**Goal**: Decode the ALPH chunk to produce the alpha plane, combine with VP8 RGB.

#### 4a. ALPH Decoder (`Alpha.hs`)

```haskell
decodeAlpha :: Int -> Int -> ByteString -> Either String (Vector Word8)
```

1. Parse header byte: reserved (2 bits, ignore), preprocessing (2 bits), filtering (2 bits), compression (2 bits)
2. If compression == 0: raw bytes, just copy `width * height` bytes
3. If compression == 1: VP8L lossless decode in "headless" mode:
   - NO signature byte, NO image size fields
   - Start directly at the transform-present bit
   - Dimensions passed externally (from VP8X or VP8 frame header)
   - After decode, extract the **green channel** as alpha values
4. Apply filtering (if filter method != 0):
   - Method 1 (horizontal): `alpha[x,y] = decoded[x,y] + alpha[x-1,y]` (mod 256)
   - Method 2 (vertical): `alpha[x,y] = decoded[x,y] + alpha[x,y-1]` (mod 256)
   - Method 3 (gradient): `alpha[x,y] = decoded[x,y] + clip(left + above - above_left)` (mod 256)
   - Border cases: top-left starts at 0; top row uses left for vertical; left column uses above for horizontal

The VP8L "headless" mode requires refactoring the VP8L decoder to accept external dimensions and skip the header. Factor out the inner decode logic:

```haskell
-- Full VP8L decode (reads header, transforms, image)
decodeVP8LFull :: ByteString -> Either String (Image PixelRGBA8)

-- Headless VP8L decode (no header, for ALPH chunk)
decodeVP8LHeaderless :: Int -> Int -> BitReader -> Either String (Vector Word32)
```

#### 4b. Integration

When the container has both `ALPH` and `VP8 ` chunks:
1. Decode alpha: `ALPH` -> `Vector Word8` (width * height)
2. Decode RGB: `VP8 ` -> `Image PixelRGB8` (width * height * 3)
3. Merge into `Image PixelRGBA8`: interleave R, G, B from VP8 with A from ALPH

### Phase 5: Animation

**Goal**: Decode animated WebP files (ANIM + ANMF frames).

```haskell
decodeWebPAnimation :: ByteString -> Either String [WebPAnimFrame]
```

1. Parse ANIM chunk: background_color (BGRA), loop_count
2. For each ANMF chunk:
   a. Parse frame header: x, y (both * 2), width, height, duration, blend, dispose
   b. Decode frame image data (nested ALPH + VP8/VP8L chunks)
   c. Produce a `WebPAnimFrame`
3. Optionally, composite onto a canvas:
   - Canvas at VP8X dimensions, initialized to background color
   - For each frame: apply previous frame's disposal, decode frame, composite at (x, y)
   - Alpha-blending formula (non-premultiplied):
     ```
     blend.A = src.A + dst.A * (1 - src.A/255)
     blend.RGB = (src.RGB * src.A + dst.RGB * dst.A * (1 - src.A/255)) / blend.A
     ```

For `decodeWebPFirstFrame`, just decode the first ANMF frame (or the single image if not animated).

Animation compositing is optional for the initial release -- returning individual frames with their metadata is sufficient for most use cases.

### Phase 6: Public API + Polish

1. Wire up `Codec.Picture.WebP` exports
2. Proper error messages with byte offsets
3. Handle all edge cases:
   - Images with 0-pixel dimensions
   - Truncated files
   - Reserved fields (ignore, don't reject)
   - Missing chunks (e.g., VP8X says alpha but no ALPH chunk)
4. Strict evaluation: force the output `Image` fully before returning (prevent space leaks from lazy decode state)
5. Add the module to `package.yaml` exposed-modules list

## Implementation Order and Rationale

| Order | Module | Why |
|-------|--------|-----|
| 1 | `BitReader.hs` | Dependency of all VP8L code |
| 2 | `Container.hs` | Dependency of everything (need to extract chunks) |
| 3 | `PrefixCode.hs` | Core VP8L primitive, self-contained |
| 4 | `LZ77.hs` | Uses PrefixCode, produces pixel arrays |
| 5 | `Transform.hs` | Post-processing of LZ77 output |
| 6 | `VP8L.hs` | Ties together 3-5, produces images. **First working decode!** |
| 7 | `Tables.hs` | Constants for VP8, no logic |
| 8 | `BoolDecoder.hs` | Core VP8 primitive, self-contained |
| 9 | `IDCT.hs` | Pure math, independently testable |
| 10 | `Dequant.hs` | Simple lookup + multiply |
| 11 | `Predict.hs` | Pixel formulas, independently testable |
| 12 | `Header.hs` | Uses BoolDecoder, produces config |
| 13 | `Coefficients.hs` | Uses BoolDecoder + Tables, most complex VP8 piece |
| 14 | `LoopFilter.hs` | Post-processing, independently testable |
| 15 | `VP8.hs` | Ties together 8-14. **Second working decode!** |
| 16 | `Alpha.hs` | Uses VP8L headless mode |
| 17 | `Animation.hs` | Uses VP8 + VP8L + Alpha |
| 18 | `WebP.hs` | Public API, ties everything together |

## Performance Considerations

- **Mutable vectors in ST**: All pixel buffers and decode state should use `MVector s` inside `ST s` or `IO`. Immutable intermediate copies would be prohibitively slow.
- **BitReader hot path**: The Huffman decode loop (readBits + table lookup) runs for every pixel. Keep the bit buffer as a `Word64`, use `unsafeIndex` for byte access.
- **BoolDecoder hot path**: Similar -- every VP8 field goes through `boolRead`. Minimize allocations, use strict fields.
- **Lookup tables for Huffman**: Two-level tables (8-bit primary + overflow) give O(1) decode. Avoid tree traversal.
- **Avoid boxed types**: Use `Int`, `Word8`, `Word16`, `Int16`, `Int32` directly. Avoid `Integer`.
- **RULES pragmas**: Consider `INLINE` on hot-path functions (`readBits`, `boolRead`, `decodeSymbol`).
- **Streaming**: Process macroblocks (VP8) or pixel rows (VP8L) sequentially. Don't build intermediate data structures for the whole image if not needed.

## Testing Strategy

1. **Unit tests**: Each module gets tests for its core functions
   - BitReader: known bit sequences
   - PrefixCode: build from known lengths, decode known bitstreams
   - BoolDecoder: known byte sequences with known probabilities
   - IDCT/WHT: known input/output pairs
   - Prediction modes: known pixel neighborhoods
   - Loop filter: edge cases (level=0, max level, etc.)

2. **Integration tests**: Compare full decode output against `dwebp` (Google's reference decoder)
   - Lossless images: pixel-exact match required
   - Lossy images: pixel-exact match required (decoder is deterministic)
   - Alpha images: pixel-exact match required
   - Animated images: each frame pixel-exact

3. **Conformance test suite**: Use WebP test images from Google's test corpus
   - Various sizes, color depths, transform combinations
   - Edge cases: 1x1 images, max-size images, all-zero, all-white
   - Corrupt files: truncated, bad signatures, invalid code lengths

4. **Property tests** (optional): QuickCheck for:
   - BitReader: read N bits, write N bits = identity
   - IDCT(DCT(x)) ~= x (within rounding tolerance)
   - Prefix code construction: valid code lengths produce valid codes
