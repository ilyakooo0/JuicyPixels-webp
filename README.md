# JuicyPixels-webp

Pure Haskell WebP decoder and encoder for JuicyPixels.

## Features

- **VP8L Lossless**: Full decode and encode support
- **VP8 Lossy**: Full decode and encode with quality control (0-100)
- **Alpha Channels**: RGBA support for both lossless and lossy
- **Animations**: Decode and encode animated WebP files
- **All Container Formats**: Simple and extended WebP

## Installation

```bash
stack build
```

## Usage

### Decoding

```haskell
import Codec.Picture.WebP
import qualified Data.ByteString as B

-- Decode any WebP file
fileData <- B.readFile "image.webp"
case decodeWebP fileData of
  Right (ImageRGB8 img)  -> -- VP8 lossy without alpha
  Right (ImageRGBA8 img) -> -- VP8L lossless or VP8 with alpha
  Left err               -> -- Handle error
```

### Encoding

```haskell
import Codec.Picture
import Codec.Picture.WebP

-- Lossless encoding
let webpData = encodeWebPLossless (ImageRGBA8 img)
B.writeFile "output.webp" webpData

-- Lossy encoding with quality (0-100)
let webpData = encodeWebPLossy 80 (ImageRGB8 img)
B.writeFile "output.webp" webpData

-- Lossy with alpha channel
let webpData = encodeWebPLossyWithAlpha 80 (ImageRGBA8 img)
B.writeFile "output.webp" webpData
```

### Animations

```haskell
-- Decode animation frames
case decodeWebPAnimation fileData of
  Right frames -> mapM_ processFrame frames
  Left err     -> handleError err

-- Encode animation
let frames = [WebPEncodeFrame img1 100 0 0, WebPEncodeFrame img2 100 0 0]
let webpData = encodeWebPAnimation frames canvasWidth canvasHeight quality
```

## Testing

```bash
stack test  # 217 tests passing
```

## API

### Decoding

- `decodeWebP :: ByteString -> Either String DynamicImage`
- `decodeWebPFirstFrame :: ByteString -> Either String DynamicImage`
- `decodeWebPAnimation :: ByteString -> Either String [WebPAnimFrame]`
- `decodeWebPAnimationComposited :: ByteString -> Either String [Image PixelRGBA8]`

### Encoding

- `encodeWebPLossless :: DynamicImage -> ByteString`
- `encodeWebPLossy :: Int -> DynamicImage -> ByteString`
- `encodeWebPLossyWithAlpha :: Int -> DynamicImage -> ByteString`
- `encodeWebPAnimation :: [WebPEncodeFrame] -> Int -> Int -> Int -> ByteString`

## Documentation

- `docs/webp-format.md` - VP8L lossless specification
- `docs/vp8-bitstream.md` - VP8 lossy specification
- `PLAN.md` - Original implementation plan

## License

BSD-3-Clause
