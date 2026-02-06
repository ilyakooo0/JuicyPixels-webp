{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8
  ( decodeVP8,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.VP8.Header
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS

-- | Decode a VP8 lossy WebP image
-- Simplified stub for now - returns a placeholder
decodeVP8 :: B.ByteString -> Either String (Image PixelRGB8)
decodeVP8 bs = do
  _header <- parseVP8Header bs

  -- Placeholder: return a 1x1 black image
  -- Full implementation would decode macroblocks, apply IDCT, loop filter, YCbCr->RGB
  let pixelData = VS.replicate 3 0
  return $ Image 1 1 pixelData
