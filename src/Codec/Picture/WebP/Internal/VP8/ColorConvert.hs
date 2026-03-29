{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.ColorConvert
  ( rgbToYCbCr,
    clip255,
  )
where

import Codec.Picture.Types
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word

-- | Clip value to [0, 255] range
{-# INLINE clip255 #-}
clip255 :: Int -> Word8
clip255 !x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = fromIntegral x

-- | Convert RGB8 image to YCbCr planes
-- Returns (Y buffer, U buffer, V buffer)
-- Y plane is full resolution (width × height)
-- U and V planes are subsampled (width/2 × height/2)
-- Uses BT.601 conversion with 4:2:0 chroma subsampling via 2×2 box filter
rgbToYCbCr ::
  Image PixelRGB8 ->
  ST s (VSM.MVector s Word8, VSM.MVector s Word8, VSM.MVector s Word8)
rgbToYCbCr img = do
  let w = imageWidth img
      h = imageHeight img
      !paddedW = ((w + 15) `shiftR` 4) `shiftL` 4
      !paddedH = ((h + 15) `shiftR` 4) `shiftL` 4
      !chromaW = paddedW `shiftR` 1
      !chromaH = paddedH `shiftR` 1

  yBuf <- VSM.replicate (paddedW * paddedH) 128
  uBuf <- VSM.replicate (chromaW * chromaH) 128
  vBuf <- VSM.replicate (chromaW * chromaH) 128

  -- Pass 1: Luma at full resolution
  forM_ [0 .. h - 1] $ \y ->
    forM_ [0 .. w - 1] $ \x -> do
      let PixelRGB8 r g b = pixelAt img x y
          -- BT.601 fixed-point (scaled by 256):
          -- Y = (77*R + 150*G + 29*B + 128) >> 8
          !r' = fromIntegral r :: Int
          !g' = fromIntegral g :: Int
          !b' = fromIntegral b :: Int
      VSM.write yBuf (y * paddedW + x) $!
        clip255 $ (77 * r' + 150 * g' + 29 * b' + 128) `shiftR` 8

  -- Pass 2: Chroma with 2×2 box filter subsampling
  -- Each chroma sample averages Cb/Cr over a 2×2 pixel block.
  -- At image edges, clamp to the nearest valid pixel.
  let !chromaRows = (h + 1) `shiftR` 1
      !chromaCols = (w + 1) `shiftR` 1
  forM_ [0 .. chromaRows - 1] $ \cy ->
    forM_ [0 .. chromaCols - 1] $ \cx -> do
      let !x0 = cx `shiftL` 1
          !y0 = cy `shiftL` 1
          !x1 = min (x0 + 1) (w - 1)
          !y1 = min (y0 + 1) (h - 1)
          -- Compute unbiased chroma per pixel (before +128 offset)
          -- Cb_raw = (-43*R - 85*G + 128*B + 128) >> 8
          -- Cr_raw = (128*R - 107*G - 21*B + 128) >> 8
          getCbCr !px !py =
            let PixelRGB8 r g b = pixelAt img px py
                !r' = fromIntegral r :: Int
                !g' = fromIntegral g :: Int
                !b' = fromIntegral b :: Int
                !cb = (-43 * r' - 85 * g' + 128 * b' + 128) `shiftR` 8
                !cr = (128 * r' - 107 * g' - 21 * b' + 128) `shiftR` 8
             in (cb, cr)
          !(cb00, cr00) = getCbCr x0 y0
          !(cb10, cr10) = getCbCr x1 y0
          !(cb01, cr01) = getCbCr x0 y1
          !(cb11, cr11) = getCbCr x1 y1
          -- Average with rounding bias (+2 for round-half-up on >>2), then add offset
          !avgCb = clip255 $ ((cb00 + cb10 + cb01 + cb11 + 2) `shiftR` 2) + 128
          !avgCr = clip255 $ ((cr00 + cr10 + cr01 + cr11 + 2) `shiftR` 2) + 128
      VSM.write uBuf (cy * chromaW + cx) avgCb
      VSM.write vBuf (cy * chromaW + cx) avgCr

  return (yBuf, uBuf, vBuf)
