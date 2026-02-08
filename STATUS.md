# Implementation Status

Pure Haskell WebP codec for JuicyPixels. Supports both decoding and encoding.

**Total Implementation:** ~9,883 lines of Haskell code across 39 modules
**Test Status:** 217 tests - all passing

---

## Fully Implemented Features

### Decoding

#### VP8L Lossless Decoder
- Full VP8L bitstream parsing with LSB-first bit reading
- Complete prefix code (Huffman) decoder with two-level lookup tables
- All 4 VP8L transforms:
  - Predictor transform (14 prediction modes)
  - Color transform (green-to-red, green-to-blue, red-to-blue deltas)
  - Subtract green (channel restoration)
  - Color indexing (palette-based pixel lookup with bit unpacking)
- LZ77 decode loop with distance map (120 entries)
- Color cache support (1-11 bits)
- Subresolution images (transforms, color cache, meta prefix codes)
- Edge cases: backreference overlapping copies, 0-bit transform terminators
- Output: `ImageRGBA8`

#### VP8 Lossy Decoder
- Complete frame header parsing (uncompressed + compressed)
- Boolean arithmetic decoder (range coder)
- Macroblock-level processing (16x16 luma, 8x8 chroma)
- Intra prediction modes:
  - 16x16 modes: DC, V, H, TM
  - 8x8 chroma modes: DC, V, H, TM
  - 4x4 luma modes: B_PRED with 10 sub-modes (DC, V, H, TM, LD, RD, VR, VL, HD, HU)
- Coefficient decoding with context-based Huffman trees
- Dequantization with lookup tables
- Complete IDCT implementation:
  - 4x4 IDCT with exact integer arithmetic
  - Walsh-Hadamard transform for Y2 DC block
  - Column-first, then row-second ordering
- Loop filter (normal and simple variants)
- YCbCr to RGB conversion
- Segmentation support (up to 4 segments)
- Output: `ImageRGB8` (or `ImageRGBA8` with alpha)

#### Alpha Channel (ALPH Chunk)
- Raw (uncompressed) alpha decoding
- VP8L-compressed alpha (headless mode)
- Alpha filtering: None, horizontal, vertical, gradient
- Green channel extraction for VP8L alpha

#### Animation (ANIM/ANMF)
- ANIM header parsing (background color, loop count)
- ANMF frame parsing (position, dimensions, duration, blend/dispose flags)
- Per-frame decoding with VP8, VP8L, or alpha data
- Animation compositing with canvas support

#### Container Parsing
- Simple lossy (VP8 only)
- Simple lossless (VP8L only)
- Extended (VP8X + chunks: VP8/VP8L, ALPH, ANIM, ANMF)
- 1-based width/height fields handled correctly
- Reserved fields ignored per spec
- RIFF chunk parsing with padding

### Encoding

#### VP8L Lossless Encoder
- Complete Huffman code generation from symbol frequencies
- Canonical Huffman construction algorithm
- Two-level lookup tables (8-bit primary + secondary)
- Support for 1-symbol, 2-symbol, and multi-symbol codes
- Code length code (CLC) encoding
- Bit writer with correct bit packing (LSB-first)
- ARGB pixel packing and histogram analysis

#### VP8 Lossy Encoder
- RGB to YCbCr color space conversion
- Macroblock mode selection and intra prediction
- DCT (Discrete Cosine Transform) for all blocks
- Coefficient quantization with quality mapping
- Boolean arithmetic encoding
- Frame header generation
- Quality parameter (0-100) mapping to quantizer values
- Padding to macroblock boundaries

#### Alpha Encoding
- Uncompressed raw alpha
- ALPH chunk creation

#### Animation Encoding
- ANIM header generation
- ANMF frame creation with metadata
- VP8X extended header with animation flag
- Per-frame VP8/VP8L encoding with optional alpha

#### Container Creation
- `makeRIFFContainer` - RIFF envelope
- `makeVP8LChunk` - VP8L chunk with padding
- `makeVP8Chunk` - VP8 chunk with padding
- `makeVP8XChunk` - extended header with flags
- `makeANIMChunk` - animation metadata
- `makeANMFChunk` - animation frame with nested chunks

---

## Future Enhancements

### VP8L Encoding Optimizations
- **Predictor transform** - predict pixels from neighbors to reduce entropy
- **Color transform** - reduce inter-channel correlation
- **Color-indexing transform** - palette-based compression for low-color images
- **Meta prefix codes** - entropy image support for better Huffman efficiency

### VP8 Encoding Optimizations
- **Loop filter** - implemented but disabled (filter level = 0)
- **Coefficient probability updates** - currently uses fixed defaults
- **Segmentation** - decoder supports it, encoder doesn't use it
- **Advanced mode selection** - currently uses simple heuristics

### Performance
- SIMD acceleration
- Streaming decode for very large images

---

## Design Notes

### Current Limitations (by design)
- No transforms in VP8L encoding: encodes pixels directly (larger files, correct output)
- Fixed coefficient probabilities in VP8 encoder
- Simple linear quality→qi mapping
- Loop filter disabled in VP8 encoder

### Error Handling
- Reserved fields read but not rejected (per RFC)
- Detailed error messages with context

### Performance Optimizations Already Applied
- Mutable vectors (ST monad) for all pixel buffers
- 64-bit bit reader buffer with `unsafeIndex`
- Two-level Huffman tables for O(1) symbol decode
- INLINE pragmas on hot-path functions

---

## API Summary

### Decoding
```haskell
decodeWebP :: ByteString -> Either String DynamicImage
decodeWebPFirstFrame :: ByteString -> Either String DynamicImage
decodeWebPAnimation :: ByteString -> Either String [WebPAnimFrame]
decodeWebPAnimationComposited :: ByteString -> Either String [Image PixelRGBA8]
```

### Encoding
```haskell
encodeWebPLossless :: Image PixelRGBA8 -> ByteString
encodeWebPLossy :: Image PixelRGB8 -> Int -> ByteString  -- quality 0-100
encodeWebPLossyWithAlpha :: Image PixelRGBA8 -> Int -> ByteString
encodeWebPAnimation :: [WebPEncodeFrame] -> Int -> Int -> Int -> ByteString
```

### Types
```haskell
data WebPAnimFrame = WebPAnimFrame
  { webpFrameImage :: DynamicImage
  , webpFrameDuration :: Int  -- milliseconds
  , webpFrameX :: Int
  , webpFrameY :: Int
  }

data WebPEncodeFrame = WebPEncodeFrame
  { webpEncodeFrameImage :: DynamicImage
  , webpEncodeFrameDuration :: Int
  , webpEncodeFrameX :: Int
  , webpEncodeFrameY :: Int
  }
```

---

## Module Structure

```
src/Codec/Picture/WebP.hs                    -- Public API

Internal/
├── Container.hs                              -- RIFF parsing, chunk dispatch
├── BitReader.hs                              -- VP8L LSB-first bit reader
├── BitWriter.hs                              -- VP8L/VP8 bit writer
│
├── VP8L/                                     -- VP8L Lossless
│   ├── PrefixCode.hs                         -- Huffman build and decode
│   ├── Transform.hs                          -- All 4 transforms
│   ├── LZ77.hs                               -- LZ77 decode loop
│   ├── EncodeComplete.hs                     -- Main encoder with Huffman
│   └── ...                                   -- Alternative encoders
│
├── VP8/                                      -- VP8 Lossy
│   ├── BoolDecoder.hs                        -- Arithmetic range coder
│   ├── BoolEncoder.hs                        -- Arithmetic range encoder
│   ├── Header.hs                             -- Frame header parsing
│   ├── Coefficients.hs                       -- Token tree decoding
│   ├── Dequant.hs                            -- Dequantization
│   ├── IDCT.hs                               -- 4x4 IDCT and WHT
│   ├── Predict.hs                            -- Intra prediction
│   ├── LoopFilter.hs                         -- Loop filter
│   ├── Tables.hs                             -- Constant tables
│   ├── ColorConvert.hs                       -- YCbCr conversions
│   ├── DCT.hs                                -- Forward DCT
│   ├── Quantize.hs                           -- Quantization
│   ├── Encode.hs                             -- VP8 encoder
│   └── ...                                   -- Encoding components
│
├── Alpha.hs                                  -- ALPH chunk decoder
├── AlphaEncode.hs                            -- ALPH chunk encoder
├── Animation.hs                              -- ANIM/ANMF decoder
├── AnimationEncode.hs                        -- ANIM/ANMF encoder
└── Encode.hs                                 -- Top-level encoding API
```
