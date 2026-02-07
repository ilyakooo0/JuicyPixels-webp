{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.ColorConvert
  ( rgbToYCbCr,
    clip255,
  )
where

import Codec.Picture.Types
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word

-- | Clip value to [0, 255] range
clip255 :: Int -> Word8
clip255 !x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = fromIntegral x

-- | Convert RGB8 image to YCbCr planes
-- Returns (Y buffer, U buffer, V buffer)
-- Y plane is full resolution (width × height)
-- U and V planes are subsampled (width/2 × height/2)
-- Uses BT.601 conversion with chroma subsampling (4:2:0)
rgbToYCbCr ::
  Image PixelRGB8 ->
  ST s (VSM.MVector s Word8, VSM.MVector s Word8, VSM.MVector s Word8)
rgbToYCbCr img = do
  let w = imageWidth img
      h = imageHeight img
      -- Pad dimensions to multiple of 16 (macroblock size)
      paddedW = ((w + 15) `div` 16) * 16
      paddedH = ((h + 15) `div` 16) * 16
      chromaW = paddedW `div` 2
      chromaH = paddedH `div` 2

  -- Allocate buffers (padded)
  yBuf <- VSM.replicate (paddedW * paddedH) 128
  uBuf <- VSM.replicate (chromaW * chromaH) 128
  vBuf <- VSM.replicate (chromaW * chromaH) 128

  -- Convert RGB to YCbCr
  forM_ [0 .. h - 1] $ \y ->
    forM_ [0 .. w - 1] $ \x -> do
      let PixelRGB8 r g b = pixelAt img x y
          -- BT.601 RGB to YCbCr conversion
          -- Y  =  0.257*R + 0.504*G + 0.098*B + 16
          -- Cb = -0.148*R - 0.291*G + 0.439*B + 128
          -- Cr =  0.439*R - 0.368*G - 0.071*B + 128
          --
          -- Using fixed-point arithmetic (scaled by 256):
          -- Y  = (66*R + 129*G + 25*B + 128) >> 8 + 16
          -- Cb = (-38*R - 74*G + 112*B + 128) >> 8 + 128
          -- Cr = (112*R - 94*G - 18*B + 128) >> 8 + 128
          r' = fromIntegral r :: Int
          g' = fromIntegral g :: Int
          b' = fromIntegral b :: Int

          y' = clip255 $ ((66 * r' + 129 * g' + 25 * b' + 128) `shiftR` 8) + 16
          cb = clip255 $ ((-38 * r' - 74 * g' + 112 * b' + 128) `shiftR` 8) + 128
          cr = clip255 $ ((112 * r' - 94 * g' - 18 * b' + 128) `shiftR` 8) + 128

      -- Write Y (full resolution)
      VSM.write yBuf (y * paddedW + x) y'

      -- Subsample chroma (simple decimation: take every other pixel)
      -- In a production encoder, you'd use a better filter (e.g., box filter or bilinear)
      when (even x && even y) $ do
        let chromaX = x `div` 2
            chromaY = y `div` 2
        VSM.write uBuf (chromaY * chromaW + chromaX) cb
        VSM.write vBuf (chromaY * chromaW + chromaX) cr

  return (yBuf, uBuf, vBuf)
