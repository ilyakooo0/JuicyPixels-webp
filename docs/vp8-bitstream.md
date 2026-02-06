# VP8 Bitstream Specification (RFC 6386) - Key Frame Decoder Reference

Reference: RFC 6386, November 2011. "VP8 Data Format and Decoding Guide."

WebP lossy images use VP8 **intra-frame (key frame) encoding only**. This document covers everything needed to decode a VP8 key frame as used within a WebP container.

## Section Index

1. [Frame Header (Uncompressed)](#frame-header-uncompressed)
2. [Boolean Arithmetic Decoder](#boolean-arithmetic-decoder-range-coder)
3. [Compressed Frame Header](#compressed-frame-header)
4. [Macroblock Structure](#macroblock-structure)
5. [Macroblock Prediction Modes](#macroblock-prediction-modes)
6. [Intra Prediction Formulas](#intra-prediction-formulas)
7. [Dequantization](#dequantization)
8. [Coefficient Decoding (Token Tree)](#coefficient-decoding-token-tree)
9. [Inverse Transforms](#inverse-transforms)
10. [Macroblock Reconstruction](#macroblock-reconstruction)
11. [MB-Level Decode Loop](#mb-level-decode-loop)
12. [Loop Filter](#loop-filter)
13. [Y'CbCr to RGB Conversion](#ycbcr-to-rgb-conversion)
14. [Complete Decoding Pipeline Summary](#complete-decoding-pipeline-summary)

## Frame Header (Uncompressed)

### Bytes 0-2: Frame Tag

```
Byte 0:
  Bit 0:     frame_type (0 = key frame, 1 = interframe)
  Bits 1-3:  version (0-3 are defined; 3 bits, 0-7 possible)
  Bit 4:     show_frame
  Bits 5-7:  first_part_size bits [2:0]

Byte 1:      first_part_size bits [10:3]
Byte 2:      first_part_size bits [18:11]
```

`first_part_size` is a 19-bit little-endian value (bits 5-23 of the 3-byte tag) giving the size of the first data partition in bytes.

For WebP, `frame_type` MUST be 0 (key frame).

### Bytes 3-9: Key Frame Header (key frames only)

```
Bytes 3-5:  Start code: 0x9D, 0x01, 0x2A
Bytes 6-7:  width_and_scale (little-endian uint16)
              Bits 0-13:  image width (14 bits)
              Bits 14-15: horizontal scale (0=none, 1=5/4, 2=5/3, 3=2x)
Bytes 8-9:  height_and_scale (little-endian uint16)
              Bits 0-13:  image height (14 bits)
              Bits 14-15: vertical scale (same as above)
```

Total uncompressed header: **10 bytes** for key frames.

### Parsing Example

```c
uint8 *data = ...; // start of VP8 chunk data

// Frame tag
uint32 tag = data[0] | (data[1] << 8) | (data[2] << 16);
int frame_type       = tag & 1;            // 0 = key frame
int version          = (tag >> 1) & 7;
int show_frame       = (tag >> 4) & 1;
int first_part_size  = (tag >> 5) & 0x7FFFF;

// Key frame header (only if frame_type == 0)
assert(data[3] == 0x9D && data[4] == 0x01 && data[5] == 0x2A);

uint16 ws = data[6] | (data[7] << 8);
int width  = ws & 0x3FFF;
int hscale = ws >> 14;

uint16 hs = data[8] | (data[9] << 8);
int height = hs & 0x3FFF;
int vscale = hs >> 14;
```

## Boolean Arithmetic Decoder (Range Coder)

VP8 uses a boolean arithmetic coder (range coder) for all compressed data.

### State

```c
typedef struct {
    const uint8 *input;  // pointer to next byte to read
    uint32 range;        // always in [128, 255] after normalization
    uint32 value;        // current coded value
    int bit_count;       // number of bits shifted out of value (0-7)
} BoolDecoder;
```

### Initialization

```c
void bool_init(BoolDecoder *d, const uint8 *data) {
    d->value = (data[0] << 8) | data[1];  // read first 2 bytes
    d->input = data + 2;
    d->range = 255;
    d->bit_count = 0;
}
```

### Reading a Boolean with Probability

```c
// prob is in [1, 255], represents probability of 0 as prob/256
int bool_read(BoolDecoder *d, int prob) {
    uint32 split = 1 + (((d->range - 1) * prob) >> 8);

    int bit;
    if (d->value < (split << 8)) {
        // bit is 0
        d->range = split;
        bit = 0;
    } else {
        // bit is 1
        d->range -= split;
        d->value -= (split << 8);
        bit = 1;
    }

    // Renormalize
    while (d->range < 128) {
        d->range <<= 1;
        d->value <<= 1;
        if (++d->bit_count == 8) {
            d->bit_count = 0;
            d->value |= *d->input++;
        }
    }
    return bit;
}
```

### Reading n-bit Unsigned Literal

```c
uint32 bool_read_literal(BoolDecoder *d, int n) {
    uint32 v = 0;
    while (n-- > 0)
        v = (v << 1) | bool_read(d, 128);  // equiprobable
    return v;
}
```

### Reading a Signed Literal

```c
int32 bool_read_signed(BoolDecoder *d, int n) {
    int32 v = bool_read_literal(d, n);
    return bool_read(d, 128) ? -v : v;  // sign bit
}
```

### Tree-Based Decoding

VP8 encodes many values using binary trees. Each internal node has an associated probability.

```c
// tree[] stores pairs of children per node: tree[node], tree[node+1]
// Negative values are leaves (value = -tree[node])
// Positive values are branch indices (next node)
int bool_read_tree(BoolDecoder *d, const int8 *tree, const uint8 *probs) {
    int i = 0;
    while ((i = tree[i + bool_read(d, probs[i >> 1])]) > 0) {}
    return -i;
}
```

## Compressed Frame Header

After the 10-byte uncompressed header, initialize a `BoolDecoder` on the first partition (size = `first_part_size`). Read the following fields in order:

### Color Space and Clamping (key frame only)

```c
int color_space = bool_read_literal(d, 1);   // 0 = YCbCr (BT.601), 1 = reserved
int clamping    = bool_read_literal(d, 1);   // 0 = clamping required
```

### Segmentation

Segment IDs (0-3) are decoded using `bool_read_tree` with this tree and the `segment_prob[3]` array:

```c
const int8 mb_segment_tree[] = {
     2,  4,      // root: "0x"=left, "1x"=right
    -0, -1,      // "00"=segment 0, "01"=segment 1
    -2, -3       // "10"=segment 2, "11"=segment 3
};
```

```c
int segmentation_enabled = bool_read_literal(d, 1);
if (segmentation_enabled) {
    int update_map   = bool_read_literal(d, 1);
    int update_data  = bool_read_literal(d, 1);
    if (update_data) {
        int segment_feature_mode = bool_read_literal(d, 1);
        // RFC 6386 Section 9.3 says: 0=absolute, 1=delta.
        // However, the reference decoder code (Section 19.2) uses the
        // opposite convention (0=delta, 1=absolute). In practice,
        // follow the reference decoder: 0=delta, 1=absolute.
        // 2 features (quantizer, loop_filter) x 4 segments
        for (int i = 0; i < 4; i++) {  // quantizer
            if (bool_read_literal(d, 1))
                segment_qp[i] = bool_read_signed(d, 7);
        }
        for (int i = 0; i < 4; i++) {  // loop filter
            if (bool_read_literal(d, 1))
                segment_lf[i] = bool_read_signed(d, 6);
        }
    }
    if (update_map) {
        for (int i = 0; i < 3; i++) {
            if (bool_read_literal(d, 1))
                segment_prob[i] = bool_read_literal(d, 8);
            else
                segment_prob[i] = 255;
        }
    }
}
```

### Loop Filter Parameters

```c
int filter_type     = bool_read_literal(d, 1);  // 0=normal, 1=simple
int filter_level    = bool_read_literal(d, 6);  // 0-63
int sharpness_level = bool_read_literal(d, 3);  // 0-7

int lf_adjust = bool_read_literal(d, 1);
if (lf_adjust) {
    int lf_delta_update = bool_read_literal(d, 1);
    if (lf_delta_update) {
        for (int i = 0; i < 4; i++) {  // ref frame deltas
            if (bool_read_literal(d, 1))
                ref_lf_delta[i] = bool_read_signed(d, 6);
        }
        for (int i = 0; i < 4; i++) {  // mode deltas
            if (bool_read_literal(d, 1))
                mode_lf_delta[i] = bool_read_signed(d, 6);
        }
    }
}
```

### Token Partitions

```c
int log2_nbr_of_dct_partitions = bool_read_literal(d, 2); // 0,1,2,3
int num_dct_partitions = 1 << log2_nbr_of_dct_partitions;  // 1,2,4,8
```

After the first bool-coded partition, partition sizes are stored as raw 3-byte little-endian values (for all partitions except the last):

```c
// Located right after the first partition
const uint8 *part_sizes_data = first_partition_data + first_part_size;
uint32 part_size[8];
for (int i = 0; i < num_dct_partitions - 1; i++) {
    part_size[i] = part_sizes_data[0]
                 | (part_sizes_data[1] << 8)
                 | (part_sizes_data[2] << 16);
    part_sizes_data += 3;
}
// Last partition extends to end of data
```

### Quantization

```c
int yac_qi    = bool_read_literal(d, 7);       // 0-127, baseline Y AC index
int ydc_delta = bool_read_literal(d, 1) ? bool_read_signed(d, 4) : 0;
int y2dc_delta = bool_read_literal(d, 1) ? bool_read_signed(d, 4) : 0;
int y2ac_delta = bool_read_literal(d, 1) ? bool_read_signed(d, 4) : 0;
int uvdc_delta = bool_read_literal(d, 1) ? bool_read_signed(d, 4) : 0;
int uvac_delta = bool_read_literal(d, 1) ? bool_read_signed(d, 4) : 0;
```

### Refresh Entropy Probabilities (key frame)

```c
int refresh_entropy_probs = bool_read_literal(d, 1);
// If 0, the coefficient probabilities are reset to defaults after
// this frame is decoded (they do NOT persist to the next frame).
// If 1, the updated probabilities persist for subsequent frames.
// For key frames this is always present; for interframes it appears
// later in the header (after reference frame flags).
```

### Coefficient Probability Updates

```c
// For each of 4 block types, 8 coeff bands, 3 contexts, 11 probabilities:
for (int t = 0; t < 4; t++)
    for (int b = 0; b < 8; b++)
        for (int c = 0; c < 3; c++)
            for (int p = 0; p < 11; p++)
                if (bool_read(d, coeff_update_probs[t][b][c][p]))
                    coeff_probs[t][b][c][p] = bool_read_literal(d, 8);
```

### Skip Coefficient Flag

```c
int prob_skip_false = 0;
int mb_no_skip_coeff = bool_read_literal(d, 1);
if (mb_no_skip_coeff)
    prob_skip_false = bool_read_literal(d, 8);
```

## Macroblock Structure

The image is divided into 16x16 macroblocks in raster-scan order.

```c
int mb_cols = (width  + 15) / 16;
int mb_rows = (height + 15) / 16;
```

Each macroblock contains:
- **Y plane**: 16x16 pixels = 4x4 grid of 4x4 subblocks (16 total)
- **U plane**: 8x8 pixels = 2x2 grid of 4x4 subblocks (4 total)
- **V plane**: 8x8 pixels = 2x2 grid of 4x4 subblocks (4 total)
- **Y2 block**: If not using B_PRED, one 4x4 block of DC values for Y (WHT-coded)

Total: 25 subblocks per macroblock (16 Y + 4 U + 4 V + 1 Y2), or 24 if B_PRED (no Y2).

### Subblock Numbering (within macroblock)

```
Y subblocks (4x4 grid):     U subblocks:    V subblocks:
 0  1  2  3                  16  17          20  21
 4  5  6  7                  18  19          22  23
 8  9  10 11
 12 13 14 15

Y2 (DC) block: 24
```

## Macroblock Prediction Modes

### Intra Y Mode (16x16)

Key frame Y mode tree (`kf_ymode_tree`) with fixed probabilities `{145, 156, 163, 128}`:

```
          root                 probs[0]=145
         /    \
     B_PRED   node1            probs[1]=156
    (code 0) /     \
          node2    node3       probs[2]=163, probs[3]=128
          /  \     /  \
        DC    V   H   TM
       100  101  110  111
```

```c
const int8 kf_ymode_tree[] = {
    -B_PRED, 2,           // root: "0"=B_PRED, "1"=subtree
     4, 6,                // node1: "10"=subtree, "11"=subtree
      -DC_PRED, -V_PRED,  // node2: "100"=DC, "101"=V
      -H_PRED, -TM_PRED   // node3: "110"=H, "111"=TM
};
```

| Value | Mode | Code | Description |
|-------|------|------|-------------|
| 0 | DC_PRED | 100 | Fill with average of above row + left column |
| 1 | V_PRED | 101 | Copy row above (vertical) |
| 2 | H_PRED | 110 | Copy left column (horizontal) |
| 3 | TM_PRED | 111 | "True motion" / gradient |
| 4 | B_PRED | 0 | Per-subblock 4x4 prediction |

### Intra B Mode (4x4 subblock, only when Y mode = B_PRED)

When Y mode is B_PRED, each of the 16 Y subblocks gets an independent mode. The mode is decoded using a tree with **context-dependent probabilities** based on the modes of the **above** and **left** neighboring subblocks.

```c
const int8 kf_bmode_tree[] = {
    -B_DC_PRED, 2,                // "0"=B_DC (0)
     -B_TM_PRED, 4,               // "10"=B_TM (1)
      -B_VE_PRED, 6,              // "110"=B_VE (2)
       8, 12,
        -B_HE_PRED, 10,           // "11100"=B_HE (3)
         -B_RD_PRED, -B_VR_PRED,  // "111010"=B_RD (5), "111011"=B_VR (6) [sic: 4,5 swapped]
        -B_LD_PRED, 14,           // "11110"=B_LD (4) [sic: out of order]
         -B_VL_PRED, 16,          // "111110"=B_VL (7)
          -B_HD_PRED, -B_HU_PRED  // "1111110"=B_HD (8), "1111111"=B_HU (9)
};
```

Note: The tree ordering differs from the enum ordering (B_RD=5 and B_LD=4 are swapped in the tree). The tree shape is: DC at root, then TM, VE, then a subtree splitting HE/RD/VR from LD/VL/HD/HU.

```c
int above_mode = ...; // mode of subblock directly above (or B_DC_PRED if top edge)
int left_mode  = ...; // mode of subblock directly left (or B_DC_PRED if left edge)
int b_mode = bool_read_tree(d, kf_bmode_tree, kf_bmode_probs[above_mode][left_mode]);
```

The 10 B-modes:

| Value | Mode | Description |
|-------|------|-------------|
| 0 | B_DC_PRED | Average of above + left edge pixels |
| 1 | B_TM_PRED | True motion (gradient) |
| 2 | B_VE_PRED | Vertical with smoothing |
| 3 | B_HE_PRED | Horizontal with smoothing |
| 4 | B_LD_PRED | Left-down diagonal (45 degrees) |
| 5 | B_RD_PRED | Right-down diagonal (45 degrees) |
| 6 | B_VR_PRED | Vertical-right (~27 degrees) |
| 7 | B_VL_PRED | Vertical-left (~27 degrees) |
| 8 | B_HD_PRED | Horizontal-down (~63 degrees) |
| 9 | B_HU_PRED | Horizontal-up (~63 degrees) |

#### `kf_bmode_probs[10][10][9]`

Context-dependent probabilities indexed by `[above_mode][left_mode]`. Each inner array has 9 probabilities for the 9 internal nodes of `kf_bmode_tree`. Source: RFC 6386 §11.5.

```c
const uint8 kf_bmode_probs[10][10][9] = {
  { /* above = B_DC_PRED (0) */
    { 231, 120,  48,  89, 115, 113, 120, 152, 112},
    { 152, 179,  64, 126, 170, 118,  46,  70,  95},
    { 175,  69, 143,  80,  85,  82,  72, 155, 103},
    {  56,  58,  10, 171, 218, 189,  17,  13, 152},
    { 144,  71,  10,  38, 171, 213, 144,  34,  26},
    { 114,  26,  17, 163,  44, 195,  21,  10, 173},
    { 121,  24,  80, 195,  26,  62,  44,  64,  85},
    { 170,  46,  55,  19, 136, 160,  33, 206,  71},
    {  63,  20,   8, 114, 114, 208,  12,   9, 226},
    {  81,  40,  11,  96, 182,  84,  29,  16,  36}
  },
  { /* above = B_TM_PRED (1) */
    { 134, 183,  89, 137,  98, 101, 106, 165, 148},
    {  72, 187, 100, 130, 157, 111,  32,  75,  80},
    {  66, 102, 167,  99,  74,  62,  40, 234, 128},
    {  41,  53,   9, 178, 241, 141,  26,   8, 107},
    { 104,  79,  12,  27, 217, 255,  87,  17,   7},
    {  74,  43,  26, 146,  73, 166,  49,  23, 157},
    {  65,  38, 105, 160,  51,  52,  31, 115, 128},
    {  87,  68,  71,  44, 114,  51,  15, 186,  23},
    {  47,  41,  14, 110, 182, 183,  21,  17, 194},
    {  66,  45,  25, 102, 197, 189,  23,  18,  22}
  },
  { /* above = B_VE_PRED (2) */
    {  88,  88, 147, 150,  42,  46,  45, 196, 205},
    {  43,  97, 183, 117,  85,  38,  35, 179,  61},
    {  39,  53, 200,  87,  26,  21,  43, 232, 171},
    {  56,  34,  51, 104, 114, 102,  29,  93,  77},
    { 107,  54,  32,  26,  51,   1,  81,  43,  31},
    {  39,  28,  85, 171,  58, 165,  90,  98,  64},
    {  34,  22, 116, 206,  23,  34,  43, 166,  73},
    {  68,  25, 106,  22,  64, 171,  36, 225, 114},
    {  34,  19,  21, 102, 132, 188,  16,  76, 124},
    {  62,  18,  78,  95,  85,  57,  50,  48,  51}
  },
  { /* above = B_HE_PRED (3) */
    { 193, 101,  35, 159, 215, 111,  89,  46, 111},
    {  60, 148,  31, 172, 219, 228,  21,  18, 111},
    { 112, 113,  77,  85, 179, 255,  38, 120, 114},
    {  40,  42,   1, 196, 245, 209,  10,  25, 109},
    { 100,  80,   8,  43, 154,   1,  51,  26,  71},
    {  88,  43,  29, 140, 166, 213,  37,  43, 154},
    {  61,  63,  30, 155,  67,  45,  68,   1, 209},
    { 142,  78,  78,  16, 255, 128,  34, 197, 171},
    {  41,  40,   5, 102, 211, 183,   4,   1, 221},
    {  51,  50,  17, 168, 209, 192,  23,  25,  82}
  },
  { /* above = B_LD_PRED (4) */
    { 125,  98,  42,  88, 104,  85, 117, 175,  82},
    {  95,  84,  53,  89, 128, 100, 113, 101,  45},
    {  75,  79, 123,  47,  51, 128,  81, 171,   1},
    {  57,  17,   5,  71, 102,  57,  53,  41,  49},
    { 115,  21,   2,  10, 102, 255, 166,  23,   6},
    {  38,  33,  13, 121,  57,  73,  26,   1,  85},
    {  41,  10,  67, 138,  77, 110,  90,  47, 114},
    { 101,  29,  16,  10,  85, 128, 101, 196,  26},
    {  57,  18,  10, 102, 102, 213,  34,  20,  43},
    { 117,  20,  15,  36, 163, 128,  68,   1,  26}
  },
  { /* above = B_RD_PRED (5) */
    { 138,  31,  36, 171,  27, 166,  38,  44, 229},
    {  67,  87,  58, 169,  82, 115,  26,  59, 179},
    {  63,  59,  90, 180,  59, 166,  93,  73, 154},
    {  40,  40,  21, 116, 143, 209,  34,  39, 175},
    {  57,  46,  22,  24, 128,   1,  54,  17,  37},
    {  47,  15,  16, 183,  34, 223,  49,  45, 183},
    {  46,  17,  33, 183,   6,  98,  15,  32, 183},
    {  65,  32,  73, 115,  28, 128,  23, 128, 205},
    {  40,   3,   9, 115,  51, 192,  18,   6, 223},
    {  87,  37,   9, 115,  59,  77,  64,  21,  47}
  },
  { /* above = B_VR_PRED (6) */
    { 104,  55,  44, 218,   9,  54,  53, 130, 226},
    {  64,  90,  70, 205,  40,  41,  23,  26,  57},
    {  54,  57, 112, 184,   5,  41,  38, 166, 213},
    {  30,  34,  26, 133, 152, 116,  10,  32, 134},
    {  75,  32,  12,  51, 192, 255, 160,  43,  51},
    {  39,  19,  53, 221,  26, 114,  32,  73, 255},
    {  31,   9,  65, 234,   2,  15,   1, 118,  73},
    {  88,  31,  35,  67, 102,  85,  55, 186,  85},
    {  56,  21,  23, 111,  59, 205,  45,  37, 192},
    {  55,  38,  70, 124,  73, 102,   1,  34,  98}
  },
  { /* above = B_VL_PRED (7) */
    { 102,  61,  71,  37,  34,  53,  31, 243, 192},
    {  69,  60,  71,  38,  73, 119,  28, 222,  37},
    {  68,  45, 128,  34,   1,  47,  11, 245, 171},
    {  62,  17,  19,  70, 146,  85,  55,  62,  70},
    {  75,  15,   9,   9,  64, 255, 184, 119,  16},
    {  37,  43,  37, 154, 100, 163,  85, 160,   1},
    {  63,   9,  92, 136,  28,  64,  32, 201,  85},
    {  86,   6,  28,   5,  64, 255,  25, 248,   1},
    {  56,   8,  17, 132, 137, 255,  55, 116, 128},
    {  58,  15,  20,  82, 135,  57,  26, 121,  40}
  },
  { /* above = B_HD_PRED (8) */
    { 164,  50,  31, 137, 154, 133,  25,  35, 218},
    {  51, 103,  44, 131, 131, 123,  31,   6, 158},
    {  86,  40,  64, 135, 148, 224,  45, 183, 128},
    {  22,  26,  17, 131, 240, 154,  14,   1, 209},
    {  83,  12,  13,  54, 192, 255,  68,  47,  28},
    {  45,  16,  21,  91,  64, 222,   7,   1, 197},
    {  56,  21,  39, 155,  60, 138,  23, 102, 213},
    {  85,  26,  85,  85, 128, 128,  32, 146, 171},
    {  18,  11,   7,  63, 144, 171,   4,   4, 246},
    {  35,  27,  10, 146, 174, 171,  12,  26, 128}
  },
  { /* above = B_HU_PRED (9) */
    { 190,  80,  35,  99, 180,  80, 126,  54,  45},
    {  85, 126,  47,  87, 176,  51,  41,  20,  32},
    { 101,  75, 128, 139, 118, 146, 116, 128,  85},
    {  56,  41,  15, 176, 236,  85,  37,   9,  62},
    { 146,  36,  19,  30, 171, 255,  97,  27,  20},
    {  71,  30,  17, 119, 118, 255,  17,  18, 138},
    { 101,  38,  60, 138,  55,  70,  43,  26, 142},
    { 138,  45,  61,  62, 219,   1,  81, 188,  64},
    {  32,  41,  20, 117, 151, 142,  20,  21, 163},
    { 112,  19,  12,  61, 195, 128,  48,   4,  24}
  }
};
```

### Chroma Mode (8x8, shared by U and V)

Key frame UV mode tree with fixed probabilities `kf_uv_mode_probs = {142, 114, 183}`:

```c
const int8 kf_uv_mode_tree[] = {
    -DC_PRED, 2,          // "0"=DC (0)
     -V_PRED, 4,          // "10"=V (1)
      -H_PRED, -TM_PRED   // "110"=H (2), "111"=TM (3)
};
```

| Value | Mode | Code |
|-------|------|------|
| 0 | DC_PRED | 0 |
| 1 | V_PRED | 10 |
| 2 | H_PRED | 110 |
| 3 | TM_PRED | 111 |

## Intra Prediction Formulas

### Edge Pixel Naming (4x4 subblock prediction)

```
    M  A  B  C  D  E  F  G  H
    --------------------------
I | p0 p1 p2 p3
J | p4 p5 p6 p7
K | p8 p9 pA pB
L | pC pD pE pF
```

- A..H: pixels from the row above (A is directly above p0)
- I..L: pixels from the column to the left (I is directly left of p0)
- M: pixel above-left (diagonal)
- `avg3(a,b,c) = (a + 2*b + c + 2) >> 2`
- `avg2(a,b) = (a + b + 1) >> 1`

### B_DC_PRED (mode 0)

Average of 4 above + 4 left pixels:

```c
int avg = 4; // rounding
for (i = 0; i < 4; i++) avg += above[i] + left[i];
avg >>= 3;
fill_block_4x4(avg);
```

Edge cases: if top-only available, average of 4 above pixels. If left-only, average of 4 left pixels. If neither, fill with 128.

### B_TM_PRED (mode 1)

```c
for (y = 0; y < 4; y++)
    for (x = 0; x < 4; x++)
        pred[y][x] = clip255(above[x] + left[y] - above_left);
```

### B_VE_PRED (mode 2) - Vertical with smoothing

Each column is filled with a smoothed value from the above row. All rows are identical.

```c
pred[*][0] = avg3(M, A, B)
pred[*][1] = avg3(A, B, C)
pred[*][2] = avg3(B, C, D)
pred[*][3] = avg3(C, D, E)
```

### B_HE_PRED (mode 3) - Horizontal with smoothing

```c
pred[0][*] = avg3(M, I, J)
pred[1][*] = avg3(I, J, K)
pred[2][*] = avg3(J, K, L)
pred[3][*] = avg3(K, L, L)  // L repeated
```

All columns are the same.

### B_LD_PRED (mode 4) - Left-down (45 degrees)

```c
pred[0][0] = avg3(A, B, C)
pred[0][1] = pred[1][0] = avg3(B, C, D)
pred[0][2] = pred[1][1] = pred[2][0] = avg3(C, D, E)
pred[0][3] = pred[1][2] = pred[2][1] = pred[3][0] = avg3(D, E, F)
pred[1][3] = pred[2][2] = pred[3][1] = avg3(E, F, G)
pred[2][3] = pred[3][2] = avg3(F, G, H)
pred[3][3] = avg3(G, H, H)  // H repeated
```

### B_RD_PRED (mode 5) - Right-down (45 degrees)

```c
pred[3][0] = avg3(J, K, L)
pred[2][0] = pred[3][1] = avg3(I, J, K)
pred[1][0] = pred[2][1] = pred[3][2] = avg3(M, I, J)
pred[0][0] = pred[1][1] = pred[2][2] = pred[3][3] = avg3(A, M, I)
pred[0][1] = pred[1][2] = pred[2][3] = avg3(B, A, M)
pred[0][2] = pred[1][3] = avg3(C, B, A)
pred[0][3] = avg3(D, C, B)
```

### B_VR_PRED (mode 6) - Vertical-right

```c
pred[0][0] = avg2(M, A)
pred[0][1] = avg2(A, B)
pred[0][2] = avg2(B, C)
pred[0][3] = avg2(C, D)
pred[1][0] = avg3(I, M, A)
pred[1][1] = avg3(M, A, B)
pred[1][2] = avg3(A, B, C)
pred[1][3] = avg3(B, C, D)
pred[2][0] = avg3(J, I, M)
pred[2][1] = pred[0][0]  // avg2(M, A)
pred[2][2] = pred[0][1]  // avg2(A, B)
pred[2][3] = pred[0][2]  // avg2(B, C)
pred[3][0] = avg3(K, J, I)
pred[3][1] = pred[1][0]  // avg3(I, M, A)
pred[3][2] = pred[1][1]  // avg3(M, A, B)
pred[3][3] = pred[1][2]  // avg3(A, B, C)
```

### B_VL_PRED (mode 7) - Vertical-left

Even rows use avg2, odd rows use avg3. Each row is shifted one position right from the row two above it. The last two cells (pred[2][3], pred[3][3]) break the strict pattern.

```c
pred[0][0] = avg2(A, B)
pred[0][1] = avg2(B, C)
pred[0][2] = avg2(C, D)
pred[0][3] = avg2(D, E)
pred[1][0] = avg3(A, B, C)
pred[1][1] = avg3(B, C, D)
pred[1][2] = avg3(C, D, E)
pred[1][3] = avg3(D, E, F)
pred[2][0] = pred[0][1]  // avg2(B, C)
pred[2][1] = pred[0][2]  // avg2(C, D)
pred[2][2] = pred[0][3]  // avg2(D, E)
pred[2][3] = avg3(E, F, G)    // breaks pattern (would be avg2(E, F))
pred[3][0] = pred[1][1]  // avg3(B, C, D)
pred[3][1] = pred[1][2]  // avg3(C, D, E)
pred[3][2] = pred[1][3]  // avg3(D, E, F)
pred[3][3] = avg3(F, G, H)    // breaks pattern (would be avg3(E, F, G))
```

### B_HD_PRED (mode 8) - Horizontal-down

```c
pred[3][0] = avg2(K, L)
pred[3][1] = avg3(J, K, L)
pred[2][0] = pred[3][2] = avg2(J, K)
pred[2][1] = pred[3][3] = avg3(I, J, K)
pred[1][0] = pred[2][2] = avg2(I, J)
pred[1][1] = pred[2][3] = avg3(M, I, J)
pred[0][0] = pred[1][2] = avg2(M, I)
pred[0][1] = pred[1][3] = avg3(A, M, I)
pred[0][2] = avg3(B, A, M)
pred[0][3] = avg3(C, B, A)
```

### B_HU_PRED (mode 9) - Horizontal-up

```c
pred[0][0] = avg2(I, J)
pred[0][1] = avg3(I, J, K)
pred[0][2] = avg2(J, K)
pred[0][3] = avg3(J, K, L)
pred[1][0] = avg2(J, K)
pred[1][1] = avg3(J, K, L)
pred[1][2] = avg2(K, L)
pred[1][3] = avg3(K, L, L)    // L repeated
pred[2][0] = avg2(K, L)
pred[2][1] = avg3(K, L, L)
pred[2][2] = pred[2][3] = pred[3][0] = pred[3][1] = pred[3][2] = pred[3][3] = L
```

### 16x16 Prediction Modes

Edge pixels: row `above[0..15]` from the reconstructed row above, column `left[0..15]` from the reconstructed column to the left, and `above_left` (the pixel at the top-left corner).

**DC_PRED** (mode 0):
```c
int sum = 16; // rounding
for (i = 0; i < 16; i++) sum += above[i] + left[i];
sum >>= 5;
fill_block_16x16(sum);
```
If only above available: average 16 above pixels. If only left: average 16 left pixels. If neither: fill with 128.

**V_PRED** (mode 1):
```c
for (y = 0; y < 16; y++)
    for (x = 0; x < 16; x++)
        pred[y][x] = above[x];
```

**H_PRED** (mode 2):
```c
for (y = 0; y < 16; y++)
    for (x = 0; x < 16; x++)
        pred[y][x] = left[y];
```

**TM_PRED** (mode 3):
```c
for (y = 0; y < 16; y++)
    for (x = 0; x < 16; x++)
        pred[y][x] = clip255(above[x] + left[y] - above_left);
```

### 8x8 Chroma Prediction

Same four modes as 16x16, applied independently to U and V planes using 8-pixel edges. DC averaging uses `sum = 8; ... sum >>= 4;` (8 pixels per edge).

## Dequantization

### Lookup Tables

Six dequantization factors are derived from the 7-bit quantizer index (QI, range 0-127):

```c
// DC quantizer for Y
static const int16 dc_qlookup[128] = {
    4,   5,   6,   7,   8,   9,  10,  10,  11,  12,  13,  14,  15,  16,  17,  17,
   18,  19,  20,  20,  21,  21,  22,  22,  23,  23,  24,  25,  25,  26,  27,  28,
   29,  30,  31,  32,  33,  34,  35,  36,  37,  37,  38,  39,  40,  41,  42,  43,
   44,  45,  46,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,
   59,  60,  61,  62,  63,  64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,
   75,  76,  76,  77,  78,  79,  80,  81,  82,  83,  84,  85,  86,  87,  88,  89,
   91,  93,  95,  96,  98, 100, 101, 102, 104, 106, 108, 110, 112, 114, 116, 118,
  122, 124, 126, 128, 130, 132, 134, 136, 138, 140, 143, 145, 148, 151, 154, 157,
};

// AC quantizer (used for Y AC, and as base for others)
static const int16 ac_qlookup[128] = {
    4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,  16,  17,  18,  19,
   20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,
   36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51,
   52,  53,  54,  55,  56,  57,  58,  60,  62,  64,  66,  68,  70,  72,  74,  76,
   78,  80,  82,  84,  86,  88,  90,  92,  94,  96,  98, 100, 102, 104, 106, 108,
  110, 112, 114, 116, 119, 122, 125, 128, 131, 134, 137, 140, 143, 146, 149, 152,
  155, 158, 161, 164, 167, 170, 173, 177, 181, 185, 189, 193, 197, 201, 205, 209,
  213, 217, 221, 225, 229, 234, 239, 245, 249, 254, 259, 264, 269, 274, 279, 284,
};
```

### Computing Quantizer Values

```c
int ydc  = dc_qlookup[clip(yac_qi + ydc_delta,  0, 127)];
int yac  = ac_qlookup[clip(yac_qi,               0, 127)];
int y2dc = dc_qlookup[clip(yac_qi + y2dc_delta, 0, 127)] * 2;
int y2ac = ac_qlookup[clip(yac_qi + y2ac_delta, 0, 127)] * 155 / 100;
// y2ac minimum is 8:
if (y2ac < 8) y2ac = 8;
int uvdc = dc_qlookup[clip(yac_qi + uvdc_delta, 0, 127)];
// uvdc maximum is 132:
if (uvdc > 132) uvdc = 132;
int uvac = ac_qlookup[clip(yac_qi + uvac_delta, 0, 127)];
```

### Zigzag Scan Order

Coefficients in a 4x4 block are stored in this order:

```c
static const int zigzag[16] = {
    0,  1,  4,  8,
    5,  2,  3,  6,
    9, 12, 13, 10,
    7, 11, 14, 15
};
```

This maps coefficient index to position in the 4x4 block (row-major: position = row*4 + col).

## Coefficient Decoding (Token Tree)

### DCT Token Values

| Token | Base Value | Extra Bits |
|-------|-----------|------------|
| DCT_0 | 0 | 0 |
| DCT_1 | 1 | 0 |
| DCT_2 | 2 | 0 |
| DCT_3 | 3 | 0 |
| DCT_4 | 4 | 0 |
| DCT_CAT1 | 5 | 1 (range 5-6) |
| DCT_CAT2 | 7 | 2 (range 7-10) |
| DCT_CAT3 | 11 | 3 (range 11-18) |
| DCT_CAT4 | 19 | 4 (range 19-34) |
| DCT_CAT5 | 35 | 5 (range 35-66) |
| DCT_CAT6 | 67 | 11 (range 67-2114) |
| DCT_EOB | end of block | - |

### Category Extra Bits Probabilities

```c
static const uint8 Pcat1[] = { 159 };
static const uint8 Pcat2[] = { 165, 145 };
static const uint8 Pcat3[] = { 173, 148, 140 };
static const uint8 Pcat4[] = { 176, 155, 140, 135 };
static const uint8 Pcat5[] = { 180, 157, 141, 134, 130 };
static const uint8 Pcat6[] = { 254, 254, 243, 230, 196, 177, 153, 140, 133, 130, 129 };
```

### Token Tree

```c
// Negative values are leaf tokens, positive values are node indices
static const int8 coeff_tree[22] = {
   -DCT_EOB, 2,                   // "0" = EOB
    -DCT_0,  4,                    // "10" = zero
     -DCT_1, 6,                    // "110" = one
      8, 12,                       // "1110..." or "1111..."
       -DCT_2, 10,                 // "11100" = two
        -DCT_3, -DCT_4,           // "111010" = three, "111011" = four
       14, 16,                     // "11110..." or "11111..."
        -DCT_CAT1, -DCT_CAT2,    // "111100" = cat1, "111101" = cat2
       18, 20,                     // "111110..." or "111111..."
        -DCT_CAT3, -DCT_CAT4,    // "1111100" = cat3, "1111101" = cat4
        -DCT_CAT5, -DCT_CAT6     // "1111110" = cat5, "1111111" = cat6
};
```

The tree encodes: at each interior node, read a bool with the corresponding probability. Branch left (0) or right (1) until reaching a leaf (negative value).

### Probability Dimensions

Coefficient probabilities are indexed by:
1. **Block type** (4 types): 0=Y after Y2, 1=Y2, 2=UV, 3=Y when B_PRED
2. **Coefficient band** (8 bands): maps coefficient position (0-15) to band (0-7)
3. **Context** (3 values): based on previously decoded token: 0=zero, 1=one, 2=two+
4. **Probability index** (11 values): one per interior tree node

```c
// Coefficient position to band mapping
static const int coeff_bands[16] = {
    0, 1, 2, 3, 6, 4, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7
};
```

### Initial Context for First Coefficient

For the first coefficient of each block, the context (0, 1, or 2) depends on whether the corresponding **above** and **left** neighbor blocks had nonzero coefficients. Maintain two arrays of flags: `above_nz[9]` (per macroblock column, reset at each row) and `left_nz[9]` (per macroblock, reset at each row start).

The mapping from subblock index (0-24) to context slot uses these index tables:

```c
// Subblock index → left context slot (0-8)
static const int left_context_index[25] = {
    0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
    4, 4, 5, 5, 6, 6, 7, 7, 8
};
// Subblock index → above context slot (0-8)
static const int above_context_index[25] = {
    0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3,
    4, 5, 4, 5, 6, 7, 6, 7, 8
};
```

Slots 0-3 = Y (4 columns/rows), 4-5 = U (2 columns/rows), 6-7 = V (2 columns/rows), 8 = Y2.

```c
// Before decoding subblock i:
int initial_ctx = left_nz[left_context_index[i]]
                + above_nz[above_context_index[i]];
// initial_ctx is 0, 1, or 2

// After decoding subblock i:
int has_nonzero = (at least one non-zero coefficient decoded);
left_nz[left_context_index[i]]  = has_nonzero;
above_nz[above_context_index[i]] = has_nonzero;
```

### Decoding a Block's Coefficients

```c
int coeffs[16] = {0};
int ctx = 0; // initial context: depends on neighbor
int start_coeff = (is_y2_coded ? 1 : 0); // skip DC if Y2 provides it
int after_zero = 0;

for (int i = start_coeff; i < 16; i++) {
    int band = coeff_bands[i];
    // After a DCT_0, the next token cannot be EOB, so skip the
    // first branch of the tree (start at coeff_tree + 2 = node 1).
    const int8 *tree_start = after_zero ? coeff_tree + 2 : coeff_tree;
    int token = bool_read_tree(d, tree_start,
                               coeff_probs[block_type][band][ctx]);
    if (token == DCT_EOB) break;
    if (token == DCT_0) {
        ctx = 0;
        after_zero = 1;
        continue;
    }
    after_zero = 0;

    int value;
    if (token <= DCT_4) {
        value = token; // 1, 2, 3, or 4
    } else {
        // Decode extra bits for category
        int cat = token - DCT_CAT1;
        const uint8 *cat_probs = category_probs[cat];
        int extra = 0;
        for (int b = 0; b < cat_extra_bits[cat]; b++)
            extra = (extra << 1) | bool_read(d, cat_probs[b]);
        value = cat_base[cat] + extra;
    }

    // Read sign bit
    if (bool_read(d, 128)) // equiprobable
        value = -value;

    coeffs[zigzag[i]] = value;
    ctx = (value == 1) ? 1 : 2;
}
```

### `coeff_update_probs[4][8][3][11]`

Probabilities used during header parsing to decide whether each coefficient probability is updated. Source: RFC 6386 §13.4.

```c
const uint8 coeff_update_probs[4][8][3][11] = {
  {
    {
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 176, 246, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 223, 241, 252, 255, 255, 255, 255, 255, 255, 255, 255},
      { 249, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 244, 252, 255, 255, 255, 255, 255, 255, 255, 255},
      { 234, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 253, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 246, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 239, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 254, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 248, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 251, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 251, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 254, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 254, 253, 255, 254, 255, 255, 255, 255, 255, 255},
      { 250, 255, 254, 255, 254, 255, 255, 255, 255, 255, 255},
      { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    }
  },
  {
    {
      { 217, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 225, 252, 241, 253, 255, 255, 254, 255, 255, 255, 255},
      { 234, 250, 241, 250, 253, 255, 253, 254, 255, 255, 255}
    },
    {
      { 255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 223, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 238, 253, 254, 254, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 248, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 249, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 253, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 247, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 252, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 253, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 254, 253, 255, 255, 255, 255, 255, 255, 255, 255},
      { 250, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    }
  },
  {
    {
      { 186, 251, 250, 255, 255, 255, 255, 255, 255, 255, 255},
      { 234, 251, 244, 254, 255, 255, 255, 255, 255, 255, 255},
      { 251, 251, 243, 253, 254, 255, 254, 255, 255, 255, 255}
    },
    {
      { 255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 236, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 251, 253, 253, 254, 254, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 254, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    }
  },
  {
    {
      { 248, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 250, 254, 252, 254, 255, 255, 255, 255, 255, 255, 255},
      { 248, 254, 249, 253, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255},
      { 246, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255},
      { 252, 254, 251, 254, 254, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 254, 252, 255, 255, 255, 255, 255, 255, 255, 255},
      { 248, 254, 253, 255, 255, 255, 255, 255, 255, 255, 255},
      { 253, 255, 254, 254, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 251, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 245, 251, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 253, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 251, 253, 255, 255, 255, 255, 255, 255, 255, 255},
      { 252, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 252, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 249, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 255, 253, 255, 255, 255, 255, 255, 255, 255, 255},
      { 250, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    },
    {
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255},
      { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255}
    }
  }
};
```

### `default_coeff_probs[4][8][3][11]`

Default token probabilities, loaded at key frame initialization. Updated via the compressed header using `coeff_update_probs` above. Source: RFC 6386 §13.5.

```c
const uint8 default_coeff_probs[4][8][3][11] = {
  { /* block type 0: Y after Y2 (Y with prediction applied via WHT) */
    {
      { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128},
      { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128},
      { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128}
    },
    {
      { 253, 136, 254, 255, 228, 219, 128, 128, 128, 128, 128},
      { 189, 129, 242, 255, 227, 213, 255, 219, 128, 128, 128},
      { 106, 126, 227, 252, 214, 209, 255, 255, 128, 128, 128}
    },
    {
      {   1,  98, 248, 255, 236, 226, 255, 255, 128, 128, 128},
      { 181, 133, 238, 254, 221, 234, 255, 154, 128, 128, 128},
      {  78, 134, 202, 247, 198, 180, 255, 219, 128, 128, 128}
    },
    {
      {   1, 185, 249, 255, 243, 255, 128, 128, 128, 128, 128},
      { 184, 150, 247, 255, 236, 224, 128, 128, 128, 128, 128},
      {  77, 110, 216, 255, 236, 230, 128, 128, 128, 128, 128}
    },
    {
      {   1, 101, 251, 255, 241, 255, 128, 128, 128, 128, 128},
      { 170, 139, 241, 252, 236, 209, 255, 255, 128, 128, 128},
      {  37, 116, 196, 243, 228, 255, 255, 255, 128, 128, 128}
    },
    {
      {   1, 204, 254, 255, 245, 255, 128, 128, 128, 128, 128},
      { 207, 160, 250, 255, 238, 128, 128, 128, 128, 128, 128},
      { 102, 103, 231, 255, 211, 171, 128, 128, 128, 128, 128}
    },
    {
      {   1, 152, 252, 255, 240, 255, 128, 128, 128, 128, 128},
      { 177, 135, 243, 255, 234, 225, 128, 128, 128, 128, 128},
      {  80, 129, 211, 255, 194, 224, 128, 128, 128, 128, 128}
    },
    {
      {   1,   1, 255, 128, 128, 128, 128, 128, 128, 128, 128},
      { 246,   1, 255, 128, 128, 128, 128, 128, 128, 128, 128},
      { 255, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128}
    }
  },
  { /* block type 1: Y2 (DC values of Y via WHT) */
    {
      { 198,  35, 237, 223, 193, 187, 162, 160, 145, 155,  62},
      { 131,  45, 198, 221, 172, 176, 220, 157, 252, 221,   1},
      {  68,  47, 146, 208, 149, 167, 221, 162, 255, 223, 128}
    },
    {
      {   1, 149, 241, 255, 221, 224, 255, 255, 128, 128, 128},
      { 184, 141, 234, 253, 222, 220, 255, 199, 128, 128, 128},
      {  81,  99, 181, 242, 176, 190, 249, 202, 255, 255, 128}
    },
    {
      {   1, 129, 232, 253, 214, 197, 242, 196, 255, 255, 128},
      {  99, 121, 210, 250, 201, 198, 255, 202, 128, 128, 128},
      {  23,  91, 163, 242, 170, 187, 247, 210, 255, 255, 128}
    },
    {
      {   1, 200, 246, 255, 234, 255, 128, 128, 128, 128, 128},
      { 109, 178, 241, 255, 231, 245, 255, 255, 128, 128, 128},
      {  44, 130, 201, 253, 205, 192, 255, 255, 128, 128, 128}
    },
    {
      {   1, 132, 239, 251, 219, 209, 255, 165, 128, 128, 128},
      {  94, 136, 225, 251, 218, 190, 255, 255, 128, 128, 128},
      {  22, 100, 174, 245, 186, 161, 255, 199, 128, 128, 128}
    },
    {
      {   1, 182, 249, 255, 232, 235, 128, 128, 128, 128, 128},
      { 124, 143, 241, 255, 227, 234, 128, 128, 128, 128, 128},
      {  35,  77, 181, 251, 193, 211, 255, 205, 128, 128, 128}
    },
    {
      {   1, 157, 247, 255, 236, 231, 255, 255, 128, 128, 128},
      { 121, 141, 235, 255, 225, 227, 255, 255, 128, 128, 128},
      {  45,  99, 188, 251, 195, 217, 255, 224, 128, 128, 128}
    },
    {
      {   1,   1, 251, 255, 213, 255, 128, 128, 128, 128, 128},
      { 203,   1, 248, 255, 255, 128, 128, 128, 128, 128, 128},
      { 137,   1, 177, 255, 224, 255, 128, 128, 128, 128, 128}
    }
  },
  { /* block type 2: UV (chroma) */
    {
      { 253,   9, 248, 251, 207, 208, 255, 192, 128, 128, 128},
      { 175,  13, 224, 243, 193, 185, 249, 198, 255, 255, 128},
      {  73,  17, 171, 221, 161, 179, 236, 167, 255, 234, 128}
    },
    {
      {   1,  95, 247, 253, 212, 183, 255, 255, 128, 128, 128},
      { 239,  90, 244, 250, 211, 209, 255, 255, 128, 128, 128},
      { 155,  77, 195, 248, 188, 195, 255, 255, 128, 128, 128}
    },
    {
      {   1,  24, 239, 251, 218, 219, 255, 205, 128, 128, 128},
      { 201,  51, 219, 255, 196, 186, 128, 128, 128, 128, 128},
      {  69,  46, 190, 239, 201, 218, 255, 228, 128, 128, 128}
    },
    {
      {   1, 191, 251, 255, 255, 128, 128, 128, 128, 128, 128},
      { 223, 165, 249, 255, 213, 255, 128, 128, 128, 128, 128},
      { 141, 124, 248, 255, 255, 128, 128, 128, 128, 128, 128}
    },
    {
      {   1,  16, 248, 255, 255, 128, 128, 128, 128, 128, 128},
      { 190,  36, 230, 255, 236, 255, 128, 128, 128, 128, 128},
      { 149,   1, 255, 128, 128, 128, 128, 128, 128, 128, 128}
    },
    {
      {   1, 226, 255, 128, 128, 128, 128, 128, 128, 128, 128},
      { 247, 192, 255, 128, 128, 128, 128, 128, 128, 128, 128},
      { 240, 128, 255, 128, 128, 128, 128, 128, 128, 128, 128}
    },
    {
      {   1, 134, 252, 255, 255, 128, 128, 128, 128, 128, 128},
      { 213,  62, 250, 255, 255, 128, 128, 128, 128, 128, 128},
      {  55,  93, 255, 128, 128, 128, 128, 128, 128, 128, 128}
    },
    {
      { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128},
      { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128},
      { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128}
    }
  },
  { /* block type 3: Y when B_PRED (no Y2, each subblock has its own DC) */
    {
      { 202,  24, 213, 235, 186, 191, 220, 160, 240, 175, 255},
      { 126,  38, 182, 232, 169, 184, 228, 174, 255, 187, 128},
      {  61,  46, 138, 219, 151, 178, 240, 170, 255, 216, 128}
    },
    {
      {   1, 112, 230, 250, 199, 191, 247, 159, 255, 255, 128},
      { 166, 109, 228, 252, 211, 215, 255, 174, 128, 128, 128},
      {  39,  77, 162, 232, 172, 180, 245, 178, 255, 255, 128}
    },
    {
      {   1,  52, 220, 246, 198, 199, 249, 220, 255, 255, 128},
      { 124,  74, 191, 243, 183, 193, 250, 221, 255, 255, 128},
      {  24,  71, 130, 219, 154, 170, 243, 182, 255, 255, 128}
    },
    {
      {   1, 182, 225, 249, 219, 240, 255, 224, 128, 128, 128},
      { 149, 150, 226, 252, 216, 205, 255, 171, 128, 128, 128},
      {  28, 108, 170, 242, 183, 194, 254, 223, 255, 255, 128}
    },
    {
      {   1,  81, 230, 252, 204, 203, 255, 192, 128, 128, 128},
      { 123, 102, 209, 247, 188, 196, 255, 233, 128, 128, 128},
      {  20,  95, 153, 243, 164, 173, 255, 203, 128, 128, 128}
    },
    {
      {   1, 222, 248, 255, 216, 213, 128, 128, 128, 128, 128},
      { 168, 175, 246, 252, 235, 205, 255, 255, 128, 128, 128},
      {  47, 116, 215, 255, 211, 212, 255, 255, 128, 128, 128}
    },
    {
      {   1, 121, 236, 253, 212, 214, 255, 255, 128, 128, 128},
      { 141,  84, 213, 252, 201, 202, 255, 219, 128, 128, 128},
      {  42,  80, 160, 240, 162, 185, 255, 205, 128, 128, 128}
    },
    {
      {   1,   1, 255, 128, 128, 128, 128, 128, 128, 128, 128},
      { 244,   1, 255, 128, 128, 128, 128, 128, 128, 128, 128},
      { 238,   1, 255, 128, 128, 128, 128, 128, 128, 128, 128}
    }
  }
};
```

## Inverse Transforms

### Inverse Walsh-Hadamard Transform (WHT, for Y2 block)

The Y2 block contains 16 DC values for the 16 Y subblocks. After dequantization, apply the inverse WHT:

```c
void iwht4x4(int16 input[16], int16 output[16]) {
    int i;
    int16 tmp[16];

    // Rows
    for (i = 0; i < 4; i++) {
        int a = input[i*4+0] + input[i*4+3];
        int b = input[i*4+1] + input[i*4+2];
        int c = input[i*4+1] - input[i*4+2];
        int d = input[i*4+0] - input[i*4+3];
        tmp[i*4+0] = a + b;
        tmp[i*4+1] = c + d;
        tmp[i*4+2] = a - b;
        tmp[i*4+3] = d - c;
    }

    // Columns
    for (i = 0; i < 4; i++) {
        int a = tmp[0*4+i] + tmp[3*4+i];
        int b = tmp[1*4+i] + tmp[2*4+i];
        int c = tmp[1*4+i] - tmp[2*4+i];
        int d = tmp[0*4+i] - tmp[3*4+i];
        output[0*4+i] = (a + b + 3) >> 3;
        output[1*4+i] = (c + d + 3) >> 3;
        output[2*4+i] = (a - b + 3) >> 3;
        output[3*4+i] = (d - c + 3) >> 3;
    }
}
```

The 16 output values are then placed as the DC coefficient (position [0][0]) of each of the 16 Y subblocks.

### Inverse DCT (4x4)

Applied to each 4x4 subblock after dequantization. Uses fixed-point integer arithmetic with specific constants:

```c
// Constants
#define cospi8sqrt2minus1  20091   // (cos(pi/8) * sqrt(2) - 1) * 65536
#define sinpi8sqrt2        35468   // sin(pi/8) * sqrt(2) * 65536

void idct4x4(int16 input[16], int16 output[16]) {
    int i;
    int16 tmp[16];

    // First pass: columns (iterate over columns, read down rows)
    for (i = 0; i < 4; i++) {
        int a = input[0*4+i] + input[2*4+i];
        int b = input[0*4+i] - input[2*4+i];

        int t1 = (input[1*4+i] * sinpi8sqrt2) >> 16;
        int t2 = input[3*4+i] + ((input[3*4+i] * cospi8sqrt2minus1) >> 16);
        int c = t1 - t2;

        t1 = input[1*4+i] + ((input[1*4+i] * cospi8sqrt2minus1) >> 16);
        t2 = (input[3*4+i] * sinpi8sqrt2) >> 16;
        int d = t1 + t2;

        tmp[0*4+i] = a + d;
        tmp[1*4+i] = b + c;
        tmp[2*4+i] = b - c;
        tmp[3*4+i] = a - d;
    }

    // Second pass: rows (iterate over rows, read across columns)
    for (i = 0; i < 4; i++) {
        int a = tmp[i*4+0] + tmp[i*4+2];
        int b = tmp[i*4+0] - tmp[i*4+2];

        int t1 = (tmp[i*4+1] * sinpi8sqrt2) >> 16;
        int t2 = tmp[i*4+3] + ((tmp[i*4+3] * cospi8sqrt2minus1) >> 16);
        int c = t1 - t2;

        t1 = tmp[i*4+1] + ((tmp[i*4+1] * cospi8sqrt2minus1) >> 16);
        t2 = (tmp[i*4+3] * sinpi8sqrt2) >> 16;
        int d = t1 + t2;

        // +4 for rounding, >>3 for normalization
        output[i*4+0] = (a + d + 4) >> 3;
        output[i*4+1] = (b + c + 4) >> 3;
        output[i*4+2] = (b - c + 4) >> 3;
        output[i*4+3] = (a - d + 4) >> 3;
    }
}
```

**Important**: The specification requires exact integer results. No floating-point approximation is allowed.

## Macroblock Reconstruction

For each subblock:

```c
// 1. Dequantize
for (int i = 0; i < 16; i++)
    coeffs[i] *= (i == 0) ? dc_quant : ac_quant;

// 2. Inverse transform (IDCT for Y/U/V subblocks, WHT for Y2)
int16 residual[16];
idct4x4(coeffs, residual);

// 3. Add residual to prediction
for (int y = 0; y < 4; y++)
    for (int x = 0; x < 4; x++)
        recon[y][x] = clip255(pred[y][x] + residual[y*4+x]);
```

### Processing Order

For non-B_PRED macroblocks:
1. Decode Y2 block (16 DC values)
2. Dequantize Y2 with y2dc/y2ac quantizers
3. Apply inverse WHT to get 16 DC values
4. For each Y subblock (0-15): place DC from WHT output, decode AC coefficients (positions 1-15), dequantize, apply inverse DCT
5. For each U subblock (16-19): decode all 16 coefficients, dequantize with uvdc/uvac, apply inverse DCT
6. For each V subblock (20-23): same as U

For B_PRED macroblocks:
1. No Y2 block. Each Y subblock has its own DC.
2. For each Y subblock: decode all 16 coefficients (including DC), dequantize with ydc/yac, apply inverse DCT
3. U and V same as above

## MB-Level Decode Loop

This ties together all previous sections into the complete per-macroblock decode pipeline. The first partition BoolDecoder (`d1`) handles modes; DCT partition BoolDecoders (`d_dct[p]`) handle coefficients. Macroblocks are assigned to DCT partitions round-robin by row: `p = mb_row % num_dct_partitions`.

```c
// Initialize context arrays (all zeros at frame start)
uint8 above_nz[mb_cols][9];  // reset each frame
memset(above_nz, 0, sizeof(above_nz));

for (int mb_row = 0; mb_row < mb_rows; mb_row++) {
    uint8 left_nz[9] = {0};  // reset each row
    BoolDecoder *d_coeff = &d_dct[mb_row % num_dct_partitions];

    for (int mb_col = 0; mb_col < mb_cols; mb_col++) {
        // --- First partition (d1): modes ---

        // 1. Segment ID
        int segment = 0;
        if (segmentation_enabled && update_map)
            segment = bool_read_tree(d1, mb_segment_tree, segment_prob);

        // 2. Skip coefficient flag
        int skip = 0;
        if (mb_no_skip_coeff)
            skip = bool_read(d1, prob_skip_false);

        // 3. Y intra mode (16x16)
        int ymode = bool_read_tree(d1, kf_ymode_tree, kf_ymode_probs);

        // 4. If B_PRED: 16 sub-block modes with context
        int bmode[16];
        if (ymode == B_PRED) {
            for (int b = 0; b < 16; b++) {
                int above = above_bmode(b, mb_row, mb_col);
                int left  = left_bmode(b, mb_row, mb_col);
                bmode[b] = bool_read_tree(d1, kf_bmode_tree,
                                          kf_bmode_probs[above][left]);
            }
        }

        // 5. Chroma mode
        int uvmode = bool_read_tree(d1, kf_uv_mode_tree, kf_uv_mode_probs);

        // --- DCT partition (d_coeff): coefficients ---

        int16 coeffs[25][16] = {0};
        int has_nz[25] = {0};

        if (!skip) {
            // 6. Decode coefficients for each subblock
            //    Block type: 0=Y after Y2, 1=Y2, 2=UV, 3=Y when B_PRED
            if (ymode != B_PRED) {
                // Y2 block (index 24, type 1, start at coeff 0)
                has_nz[24] = decode_block(d_coeff, coeffs[24], /*type*/1,
                    /*start*/0, left_nz[8], above_nz[mb_col][8]);
                left_nz[8] = above_nz[mb_col][8] = has_nz[24];
            }
            for (int b = 0; b < 16; b++) {  // Y subblocks
                int type  = (ymode == B_PRED) ? 3 : 0;
                int start = (ymode == B_PRED) ? 0 : 1;  // skip DC if Y2
                int li = left_context_index[b];
                int ai = above_context_index[b];
                has_nz[b] = decode_block(d_coeff, coeffs[b], type,
                    start, left_nz[li], above_nz[mb_col][ai]);
                left_nz[li] = above_nz[mb_col][ai] = has_nz[b];
            }
            for (int b = 16; b < 24; b++) {  // U (16-19), V (20-23)
                int li = left_context_index[b];
                int ai = above_context_index[b];
                has_nz[b] = decode_block(d_coeff, coeffs[b], /*type*/2,
                    /*start*/0, left_nz[li], above_nz[mb_col][ai]);
                left_nz[li] = above_nz[mb_col][ai] = has_nz[b];
            }
        } else {
            // skip_coeff: reset context to 0
            for (int i = 0; i < 9; i++) {
                left_nz[i] = 0;
                above_nz[mb_col][i] = 0;
            }
        }

        // 7. Dequantize + inverse transforms
        if (ymode != B_PRED) {
            dequant(coeffs[24], y2dc, y2ac);
            int16 dc_vals[16];
            iwht4x4(coeffs[24], dc_vals);
            for (int b = 0; b < 16; b++)
                coeffs[b][0] = dc_vals[b];  // place WHT output as DC
        }
        for (int b = 0; b < 16; b++) {
            dequant(coeffs[b], ydc, yac);
            idct4x4_add(coeffs[b], &y_pred[b], &y_recon[b]);
        }
        for (int b = 16; b < 24; b++) {
            dequant(coeffs[b], uvdc, uvac);
            idct4x4_add(coeffs[b], &uv_pred[b], &uv_recon[b]);
        }
    }
}

// 8. Apply loop filter to entire reconstructed frame
// 9. Convert Y'CbCr to RGB
```

## Loop Filter

### Computing Filter Limits

```c
int interior_limit, mb_edge_limit, sub_edge_limit, hev_threshold;

// Base level (may be adjusted per-segment)
int level = filter_level;

// Compute interior limit
interior_limit = level;
if (sharpness_level > 0) {
    interior_limit >>= (sharpness_level > 4) ? 2 : 1;
    if (interior_limit > 9 - sharpness_level)
        interior_limit = 9 - sharpness_level;
}
if (interior_limit < 1)
    interior_limit = 1;

// Macroblock edge limit (stronger, used at MB boundaries)
int mb_edge_limit = ((level + 2) * 2) + interior_limit;

// Subblock edge limit (used at subblock boundaries within a MB)
int sub_edge_limit = (level * 2) + interior_limit;

// HEV threshold (for normal filter; these are key frame values)
if (level >= 40)
    hev_threshold = 2;
else if (level >= 15)
    hev_threshold = 1;
else
    hev_threshold = 0;
```

### Simple Loop Filter

Applied to luma only, at macroblock and subblock edges. Tests 2 pixels on each side.

```c
void simple_filter(uint8 *p1, uint8 *p0, uint8 *q0, uint8 *q1, int limit) {
    // Only filter if edge is not too strong
    if (2 * abs(*p0 - *q0) + (abs(*p1 - *q1) >> 1) > limit)
        return;

    // Note: inner clamp on (p1-q1) before combining with 3*(q0-p0)
    int filter = clip_signed(
        clip_signed(*p1 - *q1, -128, 127) + 3 * (*q0 - *p0),
        -128, 127);
    int Filter1 = clip_signed(filter + 4, -128, 127) >> 3;
    int Filter2 = clip_signed(filter + 3, -128, 127) >> 3;

    *q0 = clip255(*q0 - Filter1);
    *p0 = clip255(*p0 + Filter2);
}
```

### Normal Loop Filter (Subblock Edges)

Tests 4 pixels on each side (p3..p0, q0..q3) for the filter_yes check, but only modifies up to 2 pixels on each side. Has two modes based on "high edge variance" (HEV).

```c
// All operations use signed arithmetic internally (u2s/s2u convert)
// c() = clip to signed 8-bit [-128, 127]
void subblock_filter(int8 *P3, int8 *P2, int8 *P1, int8 *P0,
                     int8 *Q0, int8 *Q1, int8 *Q2, int8 *Q3,
                     int edge_limit, int interior_limit, int hev_threshold) {
    int8 p3=u2s(*P3), p2=u2s(*P2), p1=u2s(*P1), p0=u2s(*P0);
    int8 q0=u2s(*Q0), q1=u2s(*Q1), q2=u2s(*Q2), q3=u2s(*Q3);

    // filter_yes: check if we should filter at all
    if ((abs(p0 - q0) * 2 + abs(p1 - q1) / 2) > edge_limit) return;
    if (abs(p3 - p2) > interior_limit) return;
    if (abs(p2 - p1) > interior_limit) return;
    if (abs(p1 - p0) > interior_limit) return;
    if (abs(q1 - q0) > interior_limit) return;
    if (abs(q2 - q1) > interior_limit) return;
    if (abs(q3 - q2) > interior_limit) return;

    if (abs(p1 - p0) > hev_threshold || abs(q1 - q0) > hev_threshold) {
        // HEV: use outer taps, modify p0/q0 only
        int8 w = c(c(p1 - q1) + 3 * (q0 - p0));
        *Q0 = s2u(q0 - (c(w + 4) >> 3));
        *P0 = s2u(p0 + (c(w + 3) >> 3));
    } else {
        // No HEV: no outer taps, modify p0/q0/p1/q1
        int8 w = c(3 * (q0 - p0));
        int8 Filter1 = c(w + 4) >> 3;
        int8 Filter2 = c(w + 3) >> 3;
        *Q0 = s2u(q0 - Filter1);
        *P0 = s2u(p0 + Filter2);
        // P1/Q1 get half of Filter1 (NOT half of raw w)
        int8 a = (Filter1 + 1) >> 1;
        *Q1 = s2u(q1 - a);
        *P1 = s2u(p1 + a);
    }
}
```

### Normal Loop Filter (Macroblock Edges)

At macroblock boundaries, the normal filter uses a wider 6-tap filter with 27/18/9 weighted adjustments (approximately 3/7, 2/7, 1/7 of the edge difference) when HEV is not detected.

```c
void MBfilter(int8 *P3, int8 *P2, int8 *P1, int8 *P0,
              int8 *Q0, int8 *Q1, int8 *Q2, int8 *Q3,
              int edge_limit, int interior_limit, int hev_threshold) {
    int8 p3=u2s(*P3), p2=u2s(*P2), p1=u2s(*P1), p0=u2s(*P0);
    int8 q0=u2s(*Q0), q1=u2s(*Q1), q2=u2s(*Q2), q3=u2s(*Q3);

    // Same filter_yes check as subblock_filter
    if ((abs(p0 - q0) * 2 + abs(p1 - q1) / 2) > edge_limit) return;
    if (abs(p3 - p2) > interior_limit) return;
    if (abs(p2 - p1) > interior_limit) return;
    if (abs(p1 - p0) > interior_limit) return;
    if (abs(q1 - q0) > interior_limit) return;
    if (abs(q2 - q1) > interior_limit) return;
    if (abs(q3 - q2) > interior_limit) return;

    if (abs(p1 - p0) > hev_threshold || abs(q1 - q0) > hev_threshold) {
        // HEV: same as subblock_filter HEV case (outer taps, p0/q0 only)
        int8 w = c(c(p1 - q1) + 3 * (q0 - p0));
        *Q0 = s2u(q0 - (c(w + 4) >> 3));
        *P0 = s2u(p0 + (c(w + 3) >> 3));
    } else {
        // No HEV: 6-tap weighted filter modifying p2/p1/p0/q0/q1/q2
        const int8 w = c(c(p1 - q1) + 3 * (q0 - p0));

        int8 a;
        a = c((27 * w + 63) >> 7);     // ~3/7 of edge difference
        *Q0 = s2u(q0 - a);  *P0 = s2u(p0 + a);

        a = c((18 * w + 63) >> 7);     // ~2/7 of edge difference
        *Q1 = s2u(q1 - a);  *P1 = s2u(p1 + a);

        a = c((9 * w + 63) >> 7);      // ~1/7 of edge difference
        *Q2 = s2u(q2 - a);  *P2 = s2u(p2 + a);
    }
}
```

### Filter Application Order

Filters are applied to:
1. Macroblock vertical edges (left edge of each macroblock, except column 0)
2. Subblock vertical edges (3 internal vertical edges per macroblock)
3. Macroblock horizontal edges (top edge of each macroblock, except row 0)
4. Subblock horizontal edges (3 internal horizontal edges per macroblock)

For simple filter: applied to Y plane only (using `simple_filter` for both MB and subblock edges).
For normal filter: applied to Y, U, and V planes (using `MBfilter` at MB edges, `subblock_filter` at subblock edges).

Macroblock edges use `mb_edge_limit`. Subblock edges use `sub_edge_limit`.

## Y'CbCr to RGB Conversion

**Note**: RFC 6386 does not specify color conversion; it is a post-processing step outside the bitstream format. The formulas below are commonly used (BT.601 studio swing) and match libvpx/libwebp behavior.

VP8 uses Y'CbCr (BT.601) color space with full-range [0, 255] values. The conversion:

```c
int R = clip255((298 * (Y - 16) + 409 * (Cr - 128) + 128) >> 8);
int G = clip255((298 * (Y - 16) - 100 * (Cb - 128) - 208 * (Cr - 128) + 128) >> 8);
int B = clip255((298 * (Y - 16) + 516 * (Cb - 128) + 128) >> 8);
```

Or equivalently (for clamping_type = 0, the common case):
```c
int c = Y - 16;
int d = Cb - 128;
int e = Cr - 128;
int R = clip255((298*c + 409*e + 128) >> 8);
int G = clip255((298*c - 100*d - 208*e + 128) >> 8);
int B = clip255((298*c + 516*d + 128) >> 8);
```

Since chroma is 4:2:0 (half resolution), U and V values must be upsampled (typically bilinear) to full resolution before conversion.

## Complete Decoding Pipeline Summary

```
1.  Parse RIFF/WebP container → extract VP8 chunk data
2.  Parse 3-byte frame tag → get frame_type, first_part_size
3.  Parse 7-byte key frame header → get width, height, start code
4.  Initialize BoolDecoder on first partition
5.  Parse compressed frame header:
    a. Color space, clamping
    b. Segmentation parameters (mb_segment_tree, segment_prob)
    c. Loop filter parameters
    d. Token partition count (log2_nbr_of_dct_partitions)
    e. Quantization indices
    f. Refresh entropy probs flag (key frame)
    g. Coefficient probability updates (coeff_update_probs → coeff_probs)
    h. Skip coefficient flag
6.  Initialize coeff_probs from default_coeff_probs, apply header updates
7.  For each macroblock in raster order (first partition + DCT partition):
    a. Read segment ID using mb_segment_tree (if segmentation enabled)
    b. Read skip_coeff flag
    c. Read Y intra mode using kf_ymode_tree
    d. If B_PRED: read 16 B-modes using kf_bmode_tree + kf_bmode_probs
    e. Read chroma mode using kf_uv_mode_tree
    f. Compute prediction buffers (16x16/8x8/4x4)
    g. Decode coefficients using coeff_tree + coeff_probs
       (with initial context from above_nz/left_nz arrays)
    h. Dequantize → inverse WHT (Y2) → inverse DCT (all subblocks)
    i. Add residual to prediction → reconstructed pixels
    (See "MB-Level Decode Loop" section for complete pseudocode)
8.  Apply loop filter to entire frame
9.  Convert Y'CbCr to RGB
10. Scale if scale factors are nonzero
```
