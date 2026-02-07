{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.DCT
  ( fdct4x4,
    fwht4x4,
  )
where

import Control.Monad.ST
import Data.Bits
import Data.Int
import qualified Data.Vector.Storable.Mutable as VSM

-- Forward DCT constants (same as inverse)
cospi8sqrt2minus1 :: Int
cospi8sqrt2minus1 = 20091

sinpi8sqrt2 :: Int
sinpi8sqrt2 = 35468

-- | 4x4 forward DCT (in-place)
-- Input: spatial domain residuals (original - prediction)
-- Output: frequency domain coefficients
-- Row pass first, then column pass (opposite order from inverse)
fdct4x4 :: VSM.MVector s Int16 -> ST s ()
fdct4x4 residuals = do
  -- Forward DCT: rows first, then columns
  fdctRow residuals 0
  fdctRow residuals 1
  fdctRow residuals 2
  fdctRow residuals 3

  fdctColumn residuals 0
  fdctColumn residuals 1
  fdctColumn residuals 2
  fdctColumn residuals 3

-- | Forward DCT row transformation
fdctRow :: VSM.MVector s Int16 -> Int -> ST s ()
fdctRow residuals row = do
  i0 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (row * 4 + 0)
  i1 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (row * 4 + 1)
  i2 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (row * 4 + 2)
  i3 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (row * 4 + 3)

  -- Scale up by 8 (to maintain precision)
  let i0' = i0 `shiftL` 3
      i1' = i1 `shiftL` 3
      i2' = i2 `shiftL` 3
      i3' = i3 `shiftL` 3

  -- Forward butterfly
  let a1 = i0' + i3'
      b1 = i1' + i2'
      c1 = i1' - i2'
      d1 = i0' - i3'

  -- Compute DCT basis
  let o0 = a1 + b1
      o2 = a1 - b1

      temp1 = (c1 * sinpi8sqrt2) `shiftR` 16
      temp2 = d1 + ((d1 * cospi8sqrt2minus1) `shiftR` 16)
      o1 = temp1 + temp2

      temp1' = c1 + ((c1 * cospi8sqrt2minus1) `shiftR` 16)
      temp2' = (d1 * sinpi8sqrt2) `shiftR` 16
      o3 = temp1' - temp2'

  VSM.write residuals (row * 4 + 0) (fromIntegral o0)
  VSM.write residuals (row * 4 + 1) (fromIntegral o1)
  VSM.write residuals (row * 4 + 2) (fromIntegral o2)
  VSM.write residuals (row * 4 + 3) (fromIntegral o3)

-- | Forward DCT column transformation (with rounding)
fdctColumn :: VSM.MVector s Int16 -> Int -> ST s ()
fdctColumn residuals col = do
  i0 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (0 * 4 + col)
  i1 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (1 * 4 + col)
  i2 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (2 * 4 + col)
  i3 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (3 * 4 + col)

  -- Forward butterfly
  let a1 = i0 + i3
      b1 = i1 + i2
      c1 = i1 - i2
      d1 = i0 - i3

  -- Compute DCT basis
  let o0 = a1 + b1
      o2 = a1 - b1

      temp1 = (c1 * sinpi8sqrt2) `shiftR` 16
      temp2 = d1 + ((d1 * cospi8sqrt2minus1) `shiftR` 16)
      o1 = temp1 + temp2

      temp1' = c1 + ((c1 * cospi8sqrt2minus1) `shiftR` 16)
      temp2' = (d1 * sinpi8sqrt2) `shiftR` 16
      o3 = temp1' - temp2'

  -- Add rounding bias and shift (divide by 4 to compensate for double 8x scaling)
  VSM.write residuals (0 * 4 + col) (fromIntegral $ (o0 + 2) `shiftR` 2)
  VSM.write residuals (1 * 4 + col) (fromIntegral $ (o1 + 2) `shiftR` 2)
  VSM.write residuals (2 * 4 + col) (fromIntegral $ (o2 + 2) `shiftR` 2)
  VSM.write residuals (3 * 4 + col) (fromIntegral $ (o3 + 2) `shiftR` 2)

-- | Forward Walsh-Hadamard Transform for Y2 DC block
-- Input: 16 DC values from the 16 Y blocks (in 4x4 arrangement)
-- Output: Transformed Y2 coefficients
-- Row pass first, then column pass (opposite order from inverse)
fwht4x4 :: VSM.MVector s Int16 -> ST s ()
fwht4x4 dcs = do
  -- Forward WHT: rows first, then columns
  fwhtRow dcs 0
  fwhtRow dcs 1
  fwhtRow dcs 2
  fwhtRow dcs 3

  fwhtColumn dcs 0
  fwhtColumn dcs 1
  fwhtColumn dcs 2
  fwhtColumn dcs 3

-- | Forward WHT row transformation
fwhtRow :: VSM.MVector s Int16 -> Int -> ST s ()
fwhtRow dcs row = do
  i0 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (row * 4 + 0)
  i1 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (row * 4 + 1)
  i2 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (row * 4 + 2)
  i3 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (row * 4 + 3)

  -- Forward butterfly (Hadamard)
  let a = i0 + i2
      b = i1 + i3
      c = i1 - i3
      d = i0 - i2

      o0 = a + b
      o1 = c + d
      o2 = a - b
      o3 = d - c

  VSM.write dcs (row * 4 + 0) (fromIntegral o0)
  VSM.write dcs (row * 4 + 1) (fromIntegral o1)
  VSM.write dcs (row * 4 + 2) (fromIntegral o2)
  VSM.write dcs (row * 4 + 3) (fromIntegral o3)

-- | Forward WHT column transformation (with rounding and scaling)
fwhtColumn :: VSM.MVector s Int16 -> Int -> ST s ()
fwhtColumn dcs col = do
  i0 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (0 * 4 + col)
  i1 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (1 * 4 + col)
  i2 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (2 * 4 + col)
  i3 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (3 * 4 + col)

  -- Forward butterfly (Hadamard)
  let a = i0 + i2
      b = i1 + i3
      c = i1 - i3
      d = i0 - i2

      -- Add rounding and shift right by 3
      o0 = (a + b + 3) `shiftR` 3
      o1 = (c + d + 3) `shiftR` 3
      o2 = (a - b + 3) `shiftR` 3
      o3 = (d - c + 3) `shiftR` 3

  VSM.write dcs (0 * 4 + col) (fromIntegral o0)
  VSM.write dcs (1 * 4 + col) (fromIntegral o1)
  VSM.write dcs (2 * 4 + col) (fromIntegral o2)
  VSM.write dcs (3 * 4 + col) (fromIntegral o3)
