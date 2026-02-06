# WebP Image Format (RFC 9649)

Reference: RFC 9649, November 2024 (Informational). Authors: J. Zern, P. Massimino, J. Alakuijala (Google LLC).

WebP is a RIFF-based image format supporting lossy (VP8) and lossless compression, alpha transparency, animation, color profiles, and metadata. It covers use cases similar to JPEG, PNG, and GIF, optimized for fast network transfer.

## Terminology & Data Types

All multi-byte integers are **little-endian**.

| Type | Description |
|------|-------------|
| `uint16` | 16-bit, little-endian, unsigned integer |
| `uint24` | 24-bit, little-endian, unsigned integer |
| `uint32` | 32-bit, little-endian, unsigned integer |
| FourCC | A `uint32` formed by concatenating four ASCII characters in little-endian order. `'aaaa'` (0x61616161) and `'AAAA'` (0x41414141) are different. |
| 1-based | An unsigned integer field storing values offset by -1. To store 25, the field contains 24. Actual value = field value + 1. |
| ChunkHeader('ABCD') | The FourCC and Chunk Size header of a chunk (8 bytes total). |

Bit numbering starts at 0 for the most significant bit (MSB 0).

A WebP file contains either a **still image** (an encoded matrix of pixels) or an **animation**. It can also contain transparency, a color profile, and metadata. The matrix of pixels is called the **canvas**.

## RIFF Chunk Structure

Every chunk in a WebP file follows the RIFF structure:

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Chunk FourCC                          |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Chunk Size                            |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
:                        Chunk Payload                          :
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

- **Chunk FourCC**: 32 bits. ASCII four-character code for chunk identification.
- **Chunk Size**: 32 bits (`uint32`). Size of the payload in bytes, excluding this field, the FourCC, and any padding.
- **Chunk Payload**: `Chunk Size` bytes. If `Chunk Size` is odd, a single padding byte MUST be appended, which MUST be 0 (per RIFF spec).

Note: RIFF convention is that uppercase FourCCs are standard and lowercase are format-specific. WebP does **not** follow this convention — its FourCCs (e.g., `'VP8X'`, `'ANIM'`, `'ALPH'`) are uppercase.

## WebP File Header

A WebP file MUST begin with a RIFF header with the FourCC `'WEBP'`. This 12-byte header:

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|      'R'      |      'I'      |      'F'      |      'F'      |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                          File Size                            |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|      'W'      |      'E'      |      'B'      |      'P'      |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

- **`'RIFF'`**: 32 bits. The ASCII characters 'R', 'I', 'F', 'F'.
- **File Size**: 32 bits (`uint32`). Size of the file in bytes, starting at offset 8. Max value is 2^32 - 10, so max file size is 4 GiB - 2 bytes. Since all chunks are padded to even size, the File Size value is always even.
- **`'WEBP'`**: 32 bits. The ASCII characters 'W', 'E', 'B', 'P'.

The file **SHOULD NOT** contain any data after the data specified by File Size. Readers **MAY** parse such files, ignoring trailing data.

**Magic bytes** (IANA registered): `52 49 46 46 xx xx xx xx 57 45 42 50 56 50 38` (i.e., `'RIFF'` + file size + `'WEBPVP8'`; where `xx` = file size bytes).

## Three File Format Variants

### 1. Simple File Format (Lossy)

Use when the image requires only lossy encoding and no transparency or extended features. Smallest and most compatible layout.

```
WebP file header (12 bytes)
'VP8 ' Chunk
```

The `'VP8 '` Chunk (note: 4th character is ASCII space 0x20):
- **VP8 data**: `Chunk Size` bytes of VP8 bitstream data (see RFC 6386).

The VP8 frame header contains width and height (assumed to be the canvas dimensions). VP8 decodes to Y'CbCr; Recommendation 601 SHOULD be used for conversion to RGB. Applications MAY use another conversion method, but visual results may differ among decoders.

### 2. Simple File Format (Lossless)

Use when the image requires lossless encoding (with optional transparency) and no extended features.

```
WebP file header (12 bytes)
'VP8L' Chunk
```

The `'VP8L'` Chunk:
- **VP8L data**: `Chunk Size` bytes of VP8L bitstream data (see Lossless Bitstream section below).

The VP8L header contains the image width and height (assumed to be canvas dimensions).

### 3. Extended File Format

Use when the image needs any combination of: lossless compression, alpha transparency, ICC color profile, Exif/XMP metadata, or animation.

```
WebP file header (12 bytes)
ChunkHeader('VP8X')
[Optional 'ICCP' Chunk]
[Optional 'ANIM' Chunk]
Image data
[Optional 'EXIF' Chunk]
[Optional 'XMP ' Chunk]
[Optional unknown chunks]
```

**Chunk ordering**: All reconstruction/color-correction chunks (`'VP8X'`, `'ICCP'`, `'ANIM'`, `'ANMF'`, `'ALPH'`, `'VP8 '`, `'VP8L'`) MUST appear in the order listed above. Readers SHOULD fail when these chunks are out of order. Metadata and unknown chunks MAY appear out of order.

## VP8X Chunk (Extended Header)

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|Rsv|I|L|E|X|A|R|                   Reserved                    |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|          Canvas Width Minus One               |      ...      |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
...  Canvas Height Minus One    |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

| Field | Bits | Description |
|-------|------|-------------|
| Reserved (Rsv) | 2 | MUST be 0. Readers MUST ignore this field. |
| ICC profile (I) | 1 | File contains an `'ICCP'` Chunk. |
| Alpha (L) | 1 | Any frame contains transparency. |
| Exif metadata (E) | 1 | File contains Exif metadata. |
| XMP metadata (X) | 1 | File contains XMP metadata. |
| Animation (A) | 1 | File is an animated image (`'ANIM'`/`'ANMF'` chunks present). |
| Reserved (R) | 1 | MUST be 0. Readers MUST ignore this field. |
| Reserved | 24 | MUST be 0. Readers MUST ignore this field. |
| Canvas Width Minus One | 24 | 1-based width. Actual width = value + 1. |
| Canvas Height Minus One | 24 | 1-based height. Actual height = value + 1. |

Canvas Width * Canvas Height MUST be at most 2^32 - 1. Future specifications may add more fields. Unknown fields MUST be ignored.

## Animation Chunks

### ANIM Chunk (Global Animation Parameters)

MUST appear if the Animation flag is set in VP8X. If the Animation flag is not set and this chunk is present, it MUST be ignored.

| Field | Type | Description |
|-------|------|-------------|
| Background Color | `uint32` | Default background in [B, G, R, A] byte order. MAY be used to fill unused canvas space and when Disposal method is 1. The background color MAY contain a nonopaque alpha value, even if the Alpha flag in VP8X is unset. Viewer apps SHOULD treat as a hint. |
| Loop Count | `uint16` | Number of times to loop the animation. 0 = infinite. For nonzero values, the animation plays Loop Count times total (repeated Loop Count - 1 times after the initial play). |

### ANMF Chunk (Animation Frame)

One per frame. SHOULD NOT be present if Animation flag is unset.

| Field | Type | Description |
|-------|------|-------------|
| Frame X | `uint24` | Upper-left X coordinate is `Frame X * 2`. |
| Frame Y | `uint24` | Upper-left Y coordinate is `Frame Y * 2`. |
| Frame Width Minus One | `uint24` | 1-based. Frame width = value + 1. |
| Frame Height Minus One | `uint24` | 1-based. Frame height = value + 1. |
| Frame Duration | `uint24` | Display time in milliseconds before showing next frame. Duration of 0 (and often <= 10) is implementation-defined. |
| Reserved | 6 bits | MUST be 0. Readers MUST ignore this field. |
| Blending method (B) | 1 bit | `0`: alpha-blend onto previous canvas (if the current frame has no alpha channel, assume alpha = 255, effectively replacing the rectangle). `1`: do not blend, overwrite the rectangle. |
| Disposal method (D) | 1 bit | `0`: do not dispose (leave canvas as is). `1`: dispose to background color (fill the *frame rectangle* with the ANIM background color). |

**Frame Data** (`Chunk Size - 16` bytes) consists of:
- An optional `'ALPH'` subchunk.
- A bitstream subchunk (`'VP8 '` or `'VP8L'`).
- An optional list of unknown chunks.

The ANMF payload consists of individual *padded* chunks per the RIFF format.

#### Alpha-blending formula

Channels are 8-bit, RGB is *not premultiplied* by alpha:

```
blend.A = src.A + dst.A * (1 - src.A / 255)
if blend.A = 0 then
  blend.RGB = 0
else
  blend.RGB =
      (src.RGB * src.A +
       dst.RGB * dst.A * (1 - src.A / 255)) / blend.A
```

Alpha-blending SHOULD be done in linear color space (linearize sRGB with gamma ~2.2 if no ICC profile is present).

#### Canvas assembly pseudocode

```
VP8X.flags.hasAnimation MUST be TRUE
canvas <- new image of size VP8X.canvasWidth x VP8X.canvasHeight with
           background color ANIM.background_color or
           application-defined color.
loop_count <- ANIM.loopCount
dispose_method <- Dispose to background color
if loop_count == 0:
  loop_count = inf
frame_params <- nil
next chunk in image_data is ANMF MUST be TRUE
for loop = 0..loop_count - 1
  clear canvas to ANIM.background_color or application-defined color
  until eof or non-ANMF chunk
    frame_params.frameX = Frame X
    frame_params.frameY = Frame Y
    frame_params.frameWidth = Frame Width Minus One + 1
    frame_params.frameHeight = Frame Height Minus One + 1
    frame_params.frameDuration = Frame Duration
    frame_right = frame_params.frameX + frame_params.frameWidth
    frame_bottom = frame_params.frameY + frame_params.frameHeight
    VP8X.canvasWidth >= frame_right MUST be TRUE
    VP8X.canvasHeight >= frame_bottom MUST be TRUE
    for subchunk in 'Frame Data':
      if subchunk.tag == "ALPH":
        alpha subchunks not found in 'Frame Data' earlier MUST be TRUE
        frame_params.alpha = alpha_data
      else if subchunk.tag == "VP8 " OR subchunk.tag == "VP8L":
        bitstream subchunks not found in 'Frame Data' earlier MUST
          be TRUE
        frame_params.bitstream = bitstream_data
    apply dispose_method
    render frame with frame_params.alpha and frame_params.bitstream
      on canvas with top-left corner at (frame_params.frameX,
      frame_params.frameY), using Blending method
      frame_params.blendingMethod.
    canvas contains the decoded image.
    Show the contents of the canvas for
    frame_params.frameDuration * 1 ms.
    dispose_method = frame_params.disposeMethod
```

## ALPH Chunk (Alpha)

```
|Rsv| P | F | C |    Alpha Bitstream...                        |
```

| Field | Bits | Description |
|-------|------|-------------|
| Reserved (Rsv) | 2 | MUST be 0. Readers MUST ignore this field. |
| Preprocessing (P) | 2 | `0`: none. `1`: level reduction. Informative only. |
| Filtering method (F) | 2 | `0`: none. `1`: horizontal. `2`: vertical. `3`: gradient. |
| Compression method (C) | 2 | `0`: no compression (raw bytes, width * height). `1`: WebP lossless format. |
| Alpha bitstream | `Chunk Size - 1` bytes | Encoded alpha data. |

A frame containing a `'VP8L'` Chunk SHOULD NOT contain this chunk (transparency is already in VP8L).

### Alpha filtering

For the pixel at position X, with neighbors:

```
C | B |
--+---+
A | X |
```

| Method | Predictor |
|--------|-----------|
| 0 | predictor = 0 |
| 1 | predictor = A (left) |
| 2 | predictor = B (above) |
| 3 | predictor = clip(A + B - C) |

where `clip(v) = 0 if v < 0, 255 if v > 255, v otherwise`.

Final alpha value: `alpha = (predictor + X) % 256`

Special cases for borders:
- Top-left pixel (0,0): predictor = 0.
- Horizontal/gradient: left-most pixels at (0, y) use pixel at (0, y-1).
- Vertical/gradient: top-most pixels at (x, 0) use pixel at (x-1, 0).

### Alpha with lossless compression

When compression method is `1`, the alpha data is a lossless-compressed image-stream of implicit dimensions (width x height) with NO headers. After decoding to ARGB, extract the alpha from the **green channel** of each pixel.

## ICCP Chunk (Color Profile)

- Contains an ICC profile (`Chunk Size` bytes).
- MUST appear before the image data.
- There SHOULD be at most one. If multiples exist, readers MAY ignore all except the first.
- If absent, sRGB SHOULD be assumed.

## Metadata Chunks

### EXIF Chunk

- `Chunk Size` bytes of Exif metadata.

### XMP Chunk

- FourCC is `'XMP '` (4th character is ASCII space 0x20).
- `Chunk Size` bytes of XMP metadata.

There SHOULD be at most one of each type. Readers MAY ignore all except the first.

## Unknown Chunks

Any RIFF chunk whose FourCC doesn't match a known type. MAY appear:
- At the end of the file (extended format).
- At the end of ANMF chunks (animation frames).

Readers SHOULD ignore unknown chunks. Writers SHOULD preserve them in their original order.

## Example File Layouts

**Lossy with alpha:**
```
RIFF/WEBP
+- VP8X (descriptions of features used)
+- ALPH (alpha bitstream)
+- VP8  (bitstream)
```

**Lossless:**
```
RIFF/WEBP
+- VP8X (descriptions of features used)
+- VP8L (lossless bitstream)
+- XYZW (unknown chunk)
```

**Lossless with ICC profile and XMP:**
```
RIFF/WEBP
+- VP8X (descriptions of features used)
+- ICCP (color profile)
+- VP8L (lossless bitstream)
+- XMP  (metadata)
```

**Animated with Exif:**
```
RIFF/WEBP
+- VP8X (descriptions of features used)
+- ANIM (global animation parameters)
+- ANMF (frame1 parameters + data)
+- ANMF (frame2 parameters + data)
+- ANMF (frame3 parameters + data)
+- ANMF (frame4 parameters + data)
+- EXIF (metadata)
```

---

# WebP Lossless Bitstream Specification

WebP lossless is a format for lossless compression of ARGB images. It stores pixel values exactly, including color values for pixels whose alpha is 0. Achieves ~25% denser compression than PNG with faster decoding.

## Bit Reading Convention

Bytes are read in natural order. Bits within each byte are read in **least-significant-bit-first** order. When multiple bits are read at once, the integer is constructed from the original data in its original order.

```c
// These are equivalent:
b = ReadBits(2);
// and:
b = ReadBits(1);
b |= ReadBits(1) << 1;
```

## ARGB Pixel Representation

Each color component (A, R, G, B) is 8 bits (`uint8`). A whole ARGB pixel is `uint32`:
- Alpha: bits 31..24
- Red: bits 23..16
- Green: bits 15..8
- Blue: bits 7..0

## RIFF Header (Lossless)

21 bytes total:
1. String `'RIFF'`.
2. `uint32` chunk length (file size minus 8 bytes: 4 for `'RIFF'` + 4 for this field).
3. String `'WEBP'`.
4. String `'VP8L'`.
5. `uint32` number of bytes in the lossless stream.
6. 1-byte signature `0x2F`.

### Image Header (after signature)

```c
int image_width  = ReadBits(14) + 1;   // max 16384
int image_height = ReadBits(14) + 1;   // max 16384
int alpha_is_used = ReadBits(1);       // hint only, SHOULD NOT affect decoding
int version_number = ReadBits(3);      // MUST be 0; any other value MUST be treated as an error
```

Maximum lossless image size: **16384 x 16384** pixels.

## Transforms

Up to four reversible transforms can be applied. Each is used at most once, only on the main-level ARGB image. Subresolution images (color transform, entropy, predictor images) have no transforms — not even the 0 bit indicating the end of transforms.

```c
while (ReadBits(1)) {           // Transform present
  enum TransformType transform_type = ReadBits(2);
  // Decode transform data...
}
// Then decode actual image data.
```

Inverse transforms are applied in **reverse order** (last read, first applied).

| Transform | Bit Value |
|-----------|-----------|
| PREDICTOR_TRANSFORM | 0 |
| COLOR_TRANSFORM | 1 |
| SUBTRACT_GREEN_TRANSFORM | 2 |
| COLOR_INDEXING_TRANSFORM | 3 |

### Predictor Transform (0)

Reduces entropy by predicting each pixel from already-decoded neighbors. Only the residual (actual - predicted) is encoded.

The image is divided into blocks. The green component of a pixel in the subresolution image selects which of 14 prediction modes to use for that block.

```c
int size_bits = ReadBits(3) + 2;
int block_width  = (1 << size_bits);
int block_height = (1 << size_bits);
#define DIV_ROUND_UP(num, den) (((num) + (den) - 1) / (den))
int transform_width = DIV_ROUND_UP(image_width, 1 << size_bits);
```

Neighboring pixels (TL = top-left, T = top, TR = top-right, L = left):

```
O  O  O  O  O  O  O  O  O  O  O
O  O  O  O  O  O  O  O  O  O  O
O  O  O  TL T  TR O  O  O  O  O
O  O  O  L  P  X  X  X  X  X  X
X  X  X  X  X  X  X  X  X  X  X
```

| Mode | Predicted Value (per channel) |
|------|------------------------------|
| 0 | 0xff000000 (solid black in ARGB) |
| 1 | L |
| 2 | T |
| 3 | TR |
| 4 | TL |
| 5 | Average2(Average2(L, TR), T) |
| 6 | Average2(L, TL) |
| 7 | Average2(L, T) |
| 8 | Average2(TL, T) |
| 9 | Average2(T, TR) |
| 10 | Average2(Average2(L, TL), Average2(T, TR)) |
| 11 | Select(L, T, TL) |
| 12 | ClampAddSubtractFull(L, T, TL) |
| 13 | ClampAddSubtractHalf(Average2(L, T), TL) |

Helper functions (per ARGB component):

```c
uint8 Average2(uint8 a, uint8 b) { return (a + b) / 2; }

int Clamp(int a) { return (a < 0) ? 0 : (a > 255) ? 255 : a; }

int ClampAddSubtractFull(int a, int b, int c) {
  return Clamp(a + b - c);
}

int ClampAddSubtractHalf(int a, int b) {
  return Clamp(a + (a - b) / 2);
}

uint32 Select(uint32 L, uint32 T, uint32 TL) {
  // L = left pixel, T = top pixel, TL = top-left pixel.

  // ARGB component estimates for prediction.
  int pAlpha = ALPHA(L) + ALPHA(T) - ALPHA(TL);
  int pRed   = RED(L)   + RED(T)   - RED(TL);
  int pGreen = GREEN(L) + GREEN(T) - GREEN(TL);
  int pBlue  = BLUE(L)  + BLUE(T)  - BLUE(TL);

  // Manhattan distances to estimates for left and top pixels.
  int pL = abs(pAlpha - ALPHA(L)) + abs(pRed - RED(L)) +
           abs(pGreen - GREEN(L)) + abs(pBlue - BLUE(L));
  int pT = abs(pAlpha - ALPHA(T)) + abs(pRed - RED(T)) +
           abs(pGreen - GREEN(T)) + abs(pBlue - BLUE(T));

  // Return either left or top, the one closer to the prediction.
  if (pL < pT) {
    return L;
  } else {
    return T;
  }
}
```

**Border pixels**: The left-topmost pixel predicts 0xff000000, top row uses L-pixel, leftmost column uses T-pixel, regardless of mode.

**Rightmost column TR exception**: Uses the leftmost pixel of the same row as TR.

Applying the predictor (inverse transform during decoding) — per-component addition modulo 256:

```c
void PredictorTransformOutput(uint32 residual, uint32 pred,
                              uint8 *alpha, uint8 *red,
                              uint8 *green, uint8 *blue) {
  *alpha = ALPHA(residual) + ALPHA(pred);
  *red   = RED(residual)   + RED(pred);
  *green = GREEN(residual) + GREEN(pred);
  *blue  = BLUE(residual)  + BLUE(pred);
}
```

### Color Transform (1)

Decorrelates R, G, B by transforming R based on G, and B based on G and R. Per block:

```c
typedef struct {
  uint8 green_to_red;
  uint8 green_to_blue;
  uint8 red_to_blue;
} ColorTransformElement;
```

Forward transform (encoding):
```c
int8 ColorTransformDelta(int8 t, int8 c) { return (t * c) >> 5; }

tmp_red  -= ColorTransformDelta(trans->green_to_red,  green);
tmp_blue -= ColorTransformDelta(trans->green_to_blue, green);
tmp_blue -= ColorTransformDelta(trans->red_to_blue,   red);
```

Inverse transform (decoding): add the deltas instead. Note the inverse uses the *already-modified* red value for `red_to_blue`:

```c
void InverseTransform(uint8 red, uint8 green, uint8 blue,
                      ColorTransformElement *trans,
                      uint8 *new_red, uint8 *new_blue) {
  int tmp_red  = red;
  int tmp_blue = blue;
  // Applying the inverse is just adding the color transform deltas
  tmp_red  += ColorTransformDelta(trans->green_to_red,  green);
  tmp_blue += ColorTransformDelta(trans->green_to_blue, green);
  tmp_blue += ColorTransformDelta(trans->red_to_blue, tmp_red & 0xff);
  *new_red  = tmp_red  & 0xff;
  *new_blue = tmp_blue & 0xff;
}
```

The `uint8` values must be reinterpreted as signed `int8` (two's complement) before calling `ColorTransformDelta`. Multiplication uses >= 16-bit precision; only lowest 8 bits of the result matter.

Block size is encoded the same way as the predictor transform. Each `ColorTransformElement` is stored as a pixel in the subresolution image with: alpha = 255, red = `red_to_blue`, green = `green_to_blue`, blue = `green_to_red`.

### Subtract Green Transform (2)

No transform data. Subtracts green from red and blue.

Inverse (decoding):
```c
*red  = (*red  + green) & 0xff;
*blue = (*blue + green) & 0xff;
```

### Color Indexing Transform (3)

Creates a palette (color table) of up to 256 ARGB values. Pixels are replaced by their index: the green channel stores the index, alpha is set to 255, and red and blue are set to 0.

```c
int color_table_size = ReadBits(8) + 1;  // 1..256
```

The color table is stored as a 1-pixel-high, `color_table_size`-wide image using the lossless format (without RIFF header, image size, or transforms). It is always subtraction-coded: in decoding, each final color is obtained by adding the previous color component values per ARGB component separately and storing the least significant 8 bits of the result.

Inverse: `argb = color_table[GREEN(argb)]`. Out-of-range index maps to 0x00000000 (transparent black).

**Pixel bundling** (when `color_table_size` <= 16): multiple pixels are packed into one pixel's green channel:

| color_table_size | width_bits | Pixels per bundle |
|------------------|------------|-------------------|
| 1..2 | 3 | 8 (1 bit each) |
| 3..4 | 2 | 4 (2 bits each) |
| 5..16 | 1 | 2 (4 bits each) |
| 17..256 | 0 | 1 (no bundling) |

Pixels are packed into the green channel in **LSB-first** order:
- `width_bits = 1`: green at `x` goes into 4 LSBs of green at `x/2`; green at `x+1` into 4 MSBs.
- `width_bits = 2`: green at `x` goes into 2 LSBs of green at `x/4`; green at `x+1..x+3` into successively higher bits.
- `width_bits = 3`: green at `x` goes into LSB of green at `x/8`; green at `x+1..x+7` into successively higher bits.

After this transform, `image_width = DIV_ROUND_UP(image_width, 1 << width_bits)`.

## Image Data

Image data is an array of pixel values in **scan-line order** (left to right, top to bottom).

### Roles of Image Data

1. **ARGB image**: The actual pixels.
2. **Entropy image**: Stores meta prefix codes (which entropy coding to use per block).
3. **Predictor image**: Prediction modes per block.
4. **Color transform image**: `ColorTransformElement` values per block.
5. **Color indexing image**: Palette (up to 256 ARGB values).

### Encoding Methods

The image is divided into fixed-size blocks (typically 16x16). Each block has its own entropy codes. Multiple blocks may share the same codes.

Each pixel is encoded as one of:
1. **Prefix-coded literals**: Green, red, blue, alpha each independently entropy-coded.
2. **LZ77 backward reference**: Copy a sequence of pixels from earlier in the image.
3. **Color cache code**: Hash-based lookup of a recently used color.

### LZ77 Prefix Coding

Large integer values (length, distance) are split into a *prefix code* (entropy-coded) and *extra bits* (stored raw).

| Value Range | Prefix Code | Extra Bits |
|-------------|-------------|------------|
| 1 | 0 | 0 |
| 2 | 1 | 0 |
| 3 | 2 | 0 |
| 4 | 3 | 0 |
| 5..6 | 4 | 1 |
| 7..8 | 5 | 1 |
| 9..12 | 6 | 2 |
| 13..16 | 7 | 2 |
| ... | ... | ... |
| 3072..4096 | 23 | 10 |
| ... | ... | ... |
| 524289..786432 | 38 | 18 |
| 786433..1048576 | 39 | 18 |

Max backward reference length: 4096. Only prefix codes 0..23 are meaningful for length. All 40 prefix codes are valid for distance.

```c
if (prefix_code < 4) return prefix_code + 1;
int extra_bits = (prefix_code - 2) >> 1;
int offset = (2 + (prefix_code & 1)) << extra_bits;
return offset + ReadBits(extra_bits) + 1;
```

### Distance Mapping

Distance codes larger than 120 denote pixel distance in scan-line order, offset by 120.

Distance codes 1..120 are reserved for a close neighborhood of the current pixel:
- Pixels 1 to 7 rows above, up to 8 columns left or 7 columns right (7 * (8+1+7) = 112 pixels).
- Pixels in the same row, up to 8 columns left (8 pixels).

The full 120-entry distance map `(xi, yi)`:
```
( 0,1), ( 1,0), ( 1,1), (-1,1), ( 0,2), ( 2,0), ( 1,2),
(-1,2), ( 2,1), (-2,1), ( 2,2), (-2,2), ( 0,3), ( 3,0),
( 1,3), (-1,3), ( 3,1), (-3,1), ( 2,3), (-2,3), ( 3,2),
(-3,2), ( 0,4), ( 4,0), ( 1,4), (-1,4), ( 4,1), (-4,1),
( 3,3), (-3,3), ( 2,4), (-2,4), ( 4,2), (-4,2), ( 0,5),
( 3,4), (-3,4), ( 4,3), (-4,3), ( 5,0), ( 1,5), (-1,5),
( 5,1), (-5,1), ( 2,5), (-2,5), ( 5,2), (-5,2), ( 4,4),
(-4,4), ( 3,5), (-3,5), ( 5,3), (-5,3), ( 0,6), ( 6,0),
( 1,6), (-1,6), ( 6,1), (-6,1), ( 2,6), (-2,6), ( 6,2),
(-6,2), ( 4,5), (-4,5), ( 5,4), (-5,4), ( 3,6), (-3,6),
( 6,3), (-6,3), ( 0,7), ( 7,0), ( 1,7), (-1,7), ( 5,5),
(-5,5), ( 7,1), (-7,1), ( 4,6), (-4,6), ( 6,4), (-6,4),
( 2,7), (-2,7), ( 7,2), (-7,2), ( 3,7), (-3,7), ( 7,3),
(-7,3), ( 5,6), (-5,6), ( 6,5), (-6,5), ( 8,0), ( 4,7),
(-4,7), ( 7,4), (-7,4), ( 8,1), ( 8,2), ( 6,6), (-6,6),
( 8,3), ( 5,7), (-5,7), ( 7,5), (-7,5), ( 8,4), ( 6,7),
(-6,7), ( 7,6), (-7,6), ( 8,5), ( 7,7), (-7,7), ( 8,6),
( 8,7)
```

Converting distance code to scan-line distance:
```c
(xi, yi) = distance_map[distance_code - 1]
dist = xi + yi * image_width
if (dist < 1) dist = 1
```

### Color Cache

A hash-addressed array storing recently used ARGB colors. First, a 1-bit flag indicates whether the color cache is used. If this bit is 0, no color cache exists and no color cache codes appear in the green/length prefix code. If the bit is 1, read the cache size:

```c
int color_cache_code_bits = ReadBits(4);  // valid range: 1..11
int color_cache_size = 1 << color_cache_code_bits;
```

`color_cache_code_bits` values outside the range [1..11] indicate a corrupted bitstream; compliant decoders MUST indicate a corrupted bitstream for such values.

Lookup: `index = (0x1e35a7bd * color) >> (32 - color_cache_code_bits)`. No conflict resolution; entries are simply overwritten. All entries are initialized to zero at the start of encoding/decoding. The cache is maintained by inserting every pixel (whether from a literal, backward reference, or cache hit) in the order they appear in the stream.

## Entropy Code

Data is coded using **canonical prefix codes** (Huffman). The format uses **spatially variant prefix coding**: different image blocks can use different entropy codes.

### Prefix Code Groups

Each pixel (x, y) is associated with a group of **5 prefix codes**:
1. **Prefix code #1**: Green channel, backward-reference length, and color cache.
2. **Prefix code #2**: Red channel.
3. **Prefix code #3**: Blue channel.
4. **Prefix code #4**: Alpha channel.
5. **Prefix code #5**: Backward-reference distance.

### Alphabet Sizes

- Green (prefix code #1): `256 + 24 + color_cache_size` symbols.
- Red, Blue, Alpha (#2, #3, #4): 256 symbols each.
- Distance (#5): 40 symbols.

A single leaf node is considered a complete binary tree and can be encoded using either simple or normal code length codes. When using the normal code length code for a single leaf, all but one code length are zeros and the single leaf value has length 1 — even though no bits are consumed when decoding from that tree. When all prefix code lengths are zeros (an empty prefix code, e.g., distance code when no backward references exist, or color channels when all pixels come from the color cache), no special handling is needed — empty prefix codes can be coded as those containing a single symbol 0.

### Simple Code Length Code

Used when only 1 or 2 symbols exist (in range [0..255]):

```c
int num_symbols = ReadBits(1) + 1;
int is_first_8bits = ReadBits(1);
symbol0 = ReadBits(1 + 7 * is_first_8bits);
code_lengths[symbol0] = 1;
if (num_symbols == 2) {
  symbol1 = ReadBits(8);
  code_lengths[symbol1] = 1;
}
```

### Normal Code Length Code

Code lengths fit in 8 bits. Read using a two-level prefix coding scheme:

```c
int num_code_lengths = 4 + ReadBits(4);  // 4..19

int kCodeLengthCodeOrder[19] = {
  17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
};
// Read 3 bits per code length, in kCodeLengthCodeOrder:
for (i = 0; i < num_code_lengths; ++i)
  code_length_code_lengths[kCodeLengthCodeOrder[i]] = ReadBits(3);
```

Then read up to `max_symbol` code lengths using:
- Code [0..15]: literal bit lengths (0 = no symbol coded).
- Code 16: repeat previous nonzero value `3 + ReadBits(2)` times (3..6). If code 16 is used before any nonzero value has been emitted, a value of 8 is repeated.
- Code 17: streak of zeros, `3 + ReadBits(3)` long (3..10).
- Code 18: streak of zeros, `11 + ReadBits(7)` long (11..138).

`max_symbol` is determined by:
```c
if (ReadBits(1) == 0)
  max_symbol = alphabet_size;  // for the respective symbol type
else {
  int length_nbits = 2 + 2 * ReadBits(3);
  max_symbol = 2 + ReadBits(length_nbits);
}
```

If `max_symbol` is larger than the alphabet size for the symbol type, the bitstream is invalid.

### Meta Prefix Codes

Allow different image blocks to use different prefix code groups. Meta prefix codes may be used **only** when the image is in the role of an ARGB image (not for subresolution images).

1-bit flag:
- `0`: One prefix code group for the entire image.
- `1`: Multiple groups, stored as an **entropy image**.

The entropy image dimensions:
```c
int prefix_bits = ReadBits(3) + 2;
int prefix_image_width  = DIV_ROUND_UP(image_width,  1 << prefix_bits);
int prefix_image_height = DIV_ROUND_UP(image_height, 1 << prefix_bits);
```

The red and green components of each pixel in the entropy image form a 16-bit meta prefix code:
```c
int meta_prefix_code = (entropy_image[position] >> 8) & 0xffff;
```

Total prefix code groups = `max(entropy image) + 1`. Total prefix codes = `5 * num_prefix_groups`.

### Decoding Entropy-Coded Image Data

For each pixel, read symbol S using prefix code #1:

1. **S < 256**: S is the green component. Then read red (#2), blue (#3), alpha (#4).
2. **256 <= S < 280**: LZ77 backward reference. `S - 256` is the length prefix code. Read extra bits for length L, then read distance prefix code (#5) and extra bits for distance D. Copy L pixels from position `current - D`.
3. **S >= 280**: Color cache index `S - 280`. Look up the ARGB color.

## Overall Structure (ABNF)

```abnf
format          = RIFF-header image-header image-stream
RIFF-header     = %s"RIFF" 4OCTET %s"WEBPVP8L" 4OCTET
image-header    = %x2F image-size alpha-is-used version
image-size      = 14BIT 14BIT ; width - 1, height - 1
alpha-is-used   = 1BIT
version         = 3BIT ; 0

image-stream    = optional-transform spatially-coded-image

optional-transform = (%b1 transform optional-transform) / %b0
transform          = predictor-tx / color-tx / subtract-green-tx
                   / color-indexing-tx

predictor-tx         = %b00 3BIT entropy-coded-image  ; sub-pixel code + predictor image
color-tx             = %b01 3BIT entropy-coded-image  ; sub-pixel code + color transform image
subtract-green-tx    = %b10                           ; no data
color-indexing-tx    = %b11 8BIT entropy-coded-image  ; color count + palette image

spatially-coded-image = color-cache-info meta-prefix data
entropy-coded-image   = color-cache-info data

color-cache-info      = %b0 / (%b1 4BIT)             ; absent, or 1 + cache size bits
meta-prefix           = %b0 / (%b1 entropy-image)    ; single group, or entropy image
entropy-image         = 3BIT entropy-coded-image      ; subsample value + image

data              = prefix-codes lz77-coded-image
prefix-codes      = prefix-code-group *prefix-codes
prefix-code-group = 5prefix-code                     ; green+len+cache, R, B, A, distance
prefix-code       = simple-prefix-code / normal-prefix-code
lz77-coded-image  = *((argb-pixel / lz77-copy / color-cache-code)
                      lz77-coded-image)
```

## Security Considerations

Implementations face risks including:
- Integer overflows, out-of-bounds reads/writes (heap and stack).
- Uninitialized data usage, null pointer dereferences.
- Resource exhaustion (disk, memory, CPU time).

The format does not employ "active content" but allows metadata (XMP, Exif) and custom chunks that may have their own security considerations.

## IANA Registration

- **Media type**: `image/webp`
- **File extension**: `.webp`
- **Magic number**: First 4 bytes `0x52 0x49 0x46 0x46` (`'RIFF'`), followed by 4 bytes for chunk size, then `0x57 0x45 0x42 0x50 0x56 0x50 0x38` (`'WEBPVP8'`).
- **Apple UTI**: `org.webmproject.webp` (conforms to `public.image`)
- **Encoding**: Binary. Use Base64 on transports that cannot handle binary directly.

## Companion Documents

- **[VP8 Lossy Bitstream](vp8-bitstream.md)**: Complete VP8 key frame decoding reference (RFC 6386), including boolean arithmetic decoder, frame header parsing, intra prediction modes (all 10 B-modes with pixel formulas), dequantization tables, inverse DCT/WHT, coefficient token decoding, loop filter, and Y'CbCr to RGB conversion. Also includes the canonical Huffman code construction algorithm needed for VP8L.
