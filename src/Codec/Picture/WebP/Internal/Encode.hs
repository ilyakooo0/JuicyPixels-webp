{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.Encode
  ( encodeWebPLossless,
    encodeWebPLossy,
    makeRIFFContainer,
    makeVP8LChunk,
    makeVP8Chunk,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.VP8L.EncodeSimple
import Codec.Picture.WebP.Internal.VP8L.EncodeComplete
import Codec.Picture.WebP.Internal.VP8L.EncodeUncompressed
import Codec.Picture.WebP.Internal.VP8L.EncodeIdentity
import Codec.Picture.WebP.Internal.VP8L.EncodeAny
import qualified Codec.Picture.WebP.Internal.VP8.Encode as VP8.Encode
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word

-- | Encode image as lossless WebP
-- Uses simple encoder which works perfectly for graphics (≤2 colors/channel)
encodeWebPLossless :: Image PixelRGBA8 -> B.ByteString
encodeWebPLossless img =
  let vp8lData = encodeVP8LSimple img
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

-- | Create VP8 chunk (stub)
makeVP8Chunk :: B.ByteString -> B.ByteString
makeVP8Chunk vp8Data =
  let fourCC = B.pack [86, 80, 56, 32] -- "VP8 "
      chunkSize = BL.toStrict $ runPut $ putWord32le (fromIntegral $ B.length vp8Data)
      padding = if odd (B.length vp8Data) then B.singleton 0 else B.empty
   in fourCC <> chunkSize <> vp8Data <> padding
