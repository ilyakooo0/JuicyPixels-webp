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
          -- Full-range YCbCr (JPEG/VP8 style) per RFC 6386
          -- Y  = 0.299*R + 0.587*G + 0.114*B
          -- Cb = -0.169*R - 0.331*G + 0.500*B + 128
          -- Cr = 0.500*R - 0.419*G - 0.081*B + 128
          --
          -- Using fixed-point arithmetic (scaled by 256):
          -- Y  = (77*R + 150*G + 29*B + 128) >> 8
          -- Cb = (-43*R - 85*G + 128*B + 128) >> 8 + 128
          -- Cr = (128*R - 107*G - 21*B + 128) >> 8 + 128
          r' = fromIntegral r :: Int
          g' = fromIntegral g :: Int
          b' = fromIntegral b :: Int

          y' = clip255 $ (77 * r' + 150 * g' + 29 * b' + 128) `shiftR` 8
          cb = clip255 $ ((-43 * r' - 85 * g' + 128 * b' + 128) `shiftR` 8) + 128
          cr = clip255 $ ((128 * r' - 107 * g' - 21 * b' + 128) `shiftR` 8) + 128

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
