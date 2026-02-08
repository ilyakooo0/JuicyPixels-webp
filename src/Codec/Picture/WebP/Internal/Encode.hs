{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.Encode
  ( encodeWebPLossless,
    encodeWebPLossy,
    encodeWebPLossyWithAlpha,
    makeRIFFContainer,
    makeVP8LChunk,
    makeVP8Chunk,
    makeVP8XChunk,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.VP8L.EncodeSimple
import Codec.Picture.WebP.Internal.VP8L.EncodeComplete
import Codec.Picture.WebP.Internal.VP8L.EncodeUncompressed
import Codec.Picture.WebP.Internal.VP8L.EncodeIdentity
import Codec.Picture.WebP.Internal.VP8L.EncodeAny
import Codec.Picture.WebP.Internal.VP8L.EncodeWorking
import qualified Codec.Picture.WebP.Internal.VP8.Encode as VP8.Encode
import Codec.Picture.WebP.Internal.AlphaEncode
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word

-- | Encode image as lossless WebP
-- Uses proper Huffman coding for true lossless compression
encodeWebPLossless :: Image PixelRGBA8 -> B.ByteString
encodeWebPLossless img =
  let vp8lData = encodeVP8LComplete img  -- Complete encoder with proper Huffman coding
      vp8lChunk = makeVP8LChunk vp8lData
      totalSize = B.length vp8lChunk
      container = makeRIFFContainer (fromIntegral totalSize) vp8lChunk
   in container

-- | Encode with uncompressed mode (for debugging)
encodeWebPLosslessUncompressed :: Image PixelRGBA8 -> B.ByteString
encodeWebPLosslessUncompressed img =
  let vp8lData = encodeVP8LUncompressed img
      vp8lChunk = makeVP8LChunk vp8lData
      totalSize = B.length vp8lChunk
      container = makeRIFFContainer (fromIntegral totalSize) vp8lChunk
   in container

-- | Encode with complete Huffman coding (for debugging)
encodeWebPLosslessComplete :: Image PixelRGBA8 -> B.ByteString
encodeWebPLosslessComplete img =
  let vp8lData = encodeVP8LComplete img
      vp8lChunk = makeVP8LChunk vp8lData
      totalSize = B.length vp8lChunk
      container = makeRIFFContainer (fromIntegral totalSize) vp8lChunk
   in container

-- | Encode image as lossy WebP using VP8 codec
-- Quality: 0-100 (higher = better quality, larger file)
--   0-30:   Low quality, small files
--   31-70:  Medium quality (recommended for web)
--   71-100: High quality, larger files
encodeWebPLossy :: Image PixelRGB8 -> Int -> B.ByteString
encodeWebPLossy img quality =
  let vp8Data = VP8.Encode.encodeVP8 img quality
      vp8Chunk = makeVP8Chunk vp8Data
      totalSize = B.length vp8Chunk
      container = makeRIFFContainer (fromIntegral totalSize) vp8Chunk
   in container

-- | Create RIFF container for WebP
makeRIFFContainer :: Word32 -> B.ByteString -> B.ByteString
makeRIFFContainer payloadSize payload =
  let riffHeader = B.pack [82, 73, 70, 70] -- "RIFF"
      fileSize = BL.toStrict $ runPut $ putWord32le (payloadSize + 4)
      webpFourCC = B.pack [87, 69, 66, 80] -- "WEBP"
   in riffHeader <> fileSize <> webpFourCC <> payload

-- | Create VP8L chunk
makeVP8LChunk :: B.ByteString -> B.ByteString
makeVP8LChunk vp8lData =
  let fourCC = B.pack [86, 80, 56, 76] -- "VP8L"
      chunkSize = BL.toStrict $ runPut $ putWord32le (fromIntegral $ B.length vp8lData)
      padding = if odd (B.length vp8lData) then B.singleton 0 else B.empty
   in fourCC <> chunkSize <> vp8lData <> padding

-- | Create VP8 chunk
makeVP8Chunk :: B.ByteString -> B.ByteString
makeVP8Chunk vp8Data =
  let fourCC = B.pack [86, 80, 56, 32] -- "VP8 "
      chunkSize = BL.toStrict $ runPut $ putWord32le (fromIntegral $ B.length vp8Data)
      padding = if odd (B.length vp8Data) then B.singleton 0 else B.empty
   in fourCC <> chunkSize <> vp8Data <> padding

-- | Create VP8X chunk (extended format header)
makeVP8XChunk ::
  Int -> Int -> -- Width, height
  Bool -> -- Has alpha
  Bool -> -- Has animation
  B.ByteString
makeVP8XChunk width height hasAlpha hasAnim =
  let fourCC = B.pack [86, 80, 56, 88] -- "VP8X"
      chunkSize = BL.toStrict $ runPut $ putWord32le 10 -- VP8X payload is always 10 bytes

      -- Flags byte:
      -- Bit 0: reserved
      -- Bit 1: has ICC profile
      -- Bit 2: has alpha
      -- Bit 3: has EXIF
      -- Bit 4: has XMP
      -- Bit 5: has animation
      -- Bits 6-7: reserved
      flags =
        ( if hasAlpha then bit 4 else 0 )
        .|. ( if hasAnim then bit 1 else 0 )

      -- Reserved 3 bytes
      reserved = B.pack [0, 0, 0]

      -- Canvas dimensions (24 bits each, minus 1)
      width24 = fromIntegral (width - 1) :: Word32
      height24 = fromIntegral (height - 1) :: Word32

      widthBytes = BL.toStrict $ runPut $ do
        putWord8 (fromIntegral $ width24 .&. 0xFF)
        putWord8 (fromIntegral $ (width24 `shiftR` 8) .&. 0xFF)
        putWord8 (fromIntegral $ (width24 `shiftR` 16) .&. 0xFF)

      heightBytes = BL.toStrict $ runPut $ do
        putWord8 (fromIntegral $ height24 .&. 0xFF)
        putWord8 (fromIntegral $ (height24 `shiftR` 8) .&. 0xFF)
        putWord8 (fromIntegral $ (height24 `shiftR` 16) .&. 0xFF)

      payload = B.singleton flags <> reserved <> widthBytes <> heightBytes

   in fourCC <> chunkSize <> payload

-- | Encode RGBA image as lossy WebP with alpha channel
encodeWebPLossyWithAlpha :: Image PixelRGBA8 -> Int -> B.ByteString
encodeWebPLossyWithAlpha img quality =
  let width = imageWidth img
      height = imageHeight img

      -- Convert RGBA to RGB (drop alpha for VP8 encoding)
      rgbImg = pixelMap (\(PixelRGBA8 r g b _) -> PixelRGB8 r g b) img

      -- Encode RGB as VP8
      vp8Data = VP8.Encode.encodeVP8 rgbImg quality
      vp8Chunk = makeVP8Chunk vp8Data

      -- Encode alpha channel as ALPH chunk
      alphaData = encodeAlpha img
      alphChunk = makeALPHChunk alphaData

      -- Create VP8X header
      vp8xChunk = makeVP8XChunk width height True False

      -- Combine chunks in extended format
      allChunks = vp8xChunk <> alphChunk <> vp8Chunk
      totalSize = B.length allChunks

   in makeRIFFContainer (fromIntegral totalSize) allChunks
