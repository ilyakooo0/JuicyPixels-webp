{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.AlphaEncode
  ( encodeAlpha,
    makeALPHChunk,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.VP8L.EncodeUncompressed
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word

-- | Encode alpha channel from RGBA image
-- Returns raw alpha data (to be wrapped in ALPH chunk)
-- For simplicity, uses uncompressed format (compression method 0)
encodeAlpha :: Image PixelRGBA8 -> B.ByteString
encodeAlpha img =
  let width = imageWidth img
      height = imageHeight img
      pixels = imageData img

      -- Extract raw alpha bytes
      alphaBytes = runST $ do
        alphaBuf <- VSM.new (width * height)

        forM_ [0 .. height - 1] $ \y ->
          forM_ [0 .. width - 1] $ \x -> do
            let pixelIdx = (y * width + x) * 4
                a = pixels VS.! (pixelIdx + 3)
                alphaIdx = y * width + x
            VSM.write alphaBuf alphaIdx a

        VS.unsafeFreeze alphaBuf
   in B.pack $ VS.toList alphaBytes

-- | Create ALPH chunk with alpha data
makeALPHChunk :: B.ByteString -> B.ByteString
makeALPHChunk alphaData =
  let fourCC = B.pack [65, 76, 80, 72] -- "ALPH"

      -- ALPH header byte:
      -- Bits 0-1: reserved (0)
      -- Bits 2-3: preprocessing (0 = none)
      -- Bits 4-5: filtering (0 = none)
      -- Bits 6-7: compression (0 = uncompressed)
      headerByte = 0x00 :: Word8 -- Compression method 0 (raw bytes)
      payload = B.cons headerByte alphaData
      chunkSize = BL.toStrict $ runPut $ putWord32le (fromIntegral $ B.length payload)
      padding = if odd (B.length payload) then B.singleton 0 else B.empty
   in fourCC <> chunkSize <> payload <> padding
