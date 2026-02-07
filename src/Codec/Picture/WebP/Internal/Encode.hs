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
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word

-- | Encode image as lossless WebP
encodeWebPLossless :: Image PixelRGBA8 -> B.ByteString
encodeWebPLossless img =
  let vp8lData = encodeVP8LSimple img
      vp8lChunk = makeVP8LChunk vp8lData
      totalSize = B.length vp8lChunk
      container = makeRIFFContainer (fromIntegral totalSize) vp8lChunk
   in container

-- | Encode image as lossy WebP (stub for now)
encodeWebPLossy :: Image PixelRGB8 -> Int -> B.ByteString
encodeWebPLossy img quality =
  error "VP8 lossy encoding not yet implemented"

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
