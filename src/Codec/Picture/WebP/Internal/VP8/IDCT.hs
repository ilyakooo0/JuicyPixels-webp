{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.IDCT
  ( idct4x4,
    iwht4x4,
  )
where

import Control.Monad (forM_)
import Control.Monad.ST
import Data.Bits
import Data.Int
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

-- Performance: INLINE pragmas on all transform functions

-- IDCT constants
cospi8sqrt2minus1 :: Int
cospi8sqrt2minus1 = 20091

sinpi8sqrt2 :: Int
sinpi8sqrt2 = 35468

-- | 4x4 inverse DCT (in-place)
-- Column pass first, then row pass
{-# INLINE idct4x4 #-}
idct4x4 :: VSM.MVector s Int16 -> ST s ()
idct4x4 coeffs = do
  idctColumn coeffs 0
  idctColumn coeffs 1
  idctColumn coeffs 2
  idctColumn coeffs 3

  idctRow coeffs 0
  idctRow coeffs 1
  idctRow coeffs 2
  idctRow coeffs 3

-- | IDCT column transformation
{-# INLINE idctColumn #-}
idctColumn :: VSM.MVector s Int16 -> Int -> ST s ()
idctColumn coeffs col = do
  -- Indices 0-15 are always valid for 4x4 block, use unsafeRead
  i0 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (0 * 4 + col)
  i1 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (1 * 4 + col)
  i2 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (2 * 4 + col)
  i3 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (3 * 4 + col)

  let a1 = i0 + i2
      b1 = i0 - i2

      temp1 = (i1 * sinpi8sqrt2) `shiftR` 16
      temp2 = i3 + ((i3 * cospi8sqrt2minus1) `shiftR` 16)
      c1 = temp1 - temp2

      temp1' = i1 + ((i1 * cospi8sqrt2minus1) `shiftR` 16)
      temp2' = (i3 * sinpi8sqrt2) `shiftR` 16
      d1 = temp1' + temp2'

      o0 = a1 + d1
      o1 = b1 + c1
      o2 = b1 - c1
      o3 = a1 - d1

  VSM.unsafeWrite coeffs (0 * 4 + col) (fromIntegral o0)
  VSM.unsafeWrite coeffs (1 * 4 + col) (fromIntegral o1)
  VSM.unsafeWrite coeffs (2 * 4 + col) (fromIntegral o2)
  VSM.unsafeWrite coeffs (3 * 4 + col) (fromIntegral o3)

-- | IDCT row transformation (with rounding)
{-# INLINE idctRow #-}
idctRow :: VSM.MVector s Int16 -> Int -> ST s ()
idctRow coeffs row = do
  -- Indices 0-15 are always valid for 4x4 block, use unsafeRead
  i0 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (row * 4 + 0)
  i1 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (row * 4 + 1)
  i2 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (row * 4 + 2)
  i3 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (row * 4 + 3)

  let a1 = i0 + i2
      b1 = i0 - i2

      temp1 = (i1 * sinpi8sqrt2) `shiftR` 16
      temp2 = i3 + ((i3 * cospi8sqrt2minus1) `shiftR` 16)
      c1 = temp1 - temp2

      temp1' = i1 + ((i1 * cospi8sqrt2minus1) `shiftR` 16)
      temp2' = (i3 * sinpi8sqrt2) `shiftR` 16
      d1 = temp1' + temp2'

      o0 = (a1 + d1 + 4) `shiftR` 3
      o1 = (b1 + c1 + 4) `shiftR` 3
      o2 = (b1 - c1 + 4) `shiftR` 3
      o3 = (a1 - d1 + 4) `shiftR` 3

  VSM.unsafeWrite coeffs (row * 4 + 0) (fromIntegral o0)
  VSM.unsafeWrite coeffs (row * 4 + 1) (fromIntegral o1)
  VSM.unsafeWrite coeffs (row * 4 + 2) (fromIntegral o2)
  VSM.unsafeWrite coeffs (row * 4 + 3) (fromIntegral o3)

-- | Walsh-Hadamard Transform for Y2 DC block
-- Returns 16 DC values to be distributed to Y subblocks
-- Per RFC 6386: rows first (no scaling), then columns (divide by 8)
{-# INLINE iwht4x4 #-}
iwht4x4 :: VSM.MVector s Int16 -> ST s (VS.Vector Int16)
iwht4x4 coeffs = do
  -- Row pass first (no scaling)
  whtRow coeffs 0
  whtRow coeffs 1
  whtRow coeffs 2
  whtRow coeffs 3

  -- Column pass second (divide by 8)
  whtColumn coeffs 0
  whtColumn coeffs 1
  whtColumn coeffs 2
  whtColumn coeffs 3

  VS.unsafeFreeze coeffs

-- | WHT row transformation (no scaling per RFC 6386 - first pass)
{-# INLINE whtRow #-}
whtRow :: VSM.MVector s Int16 -> Int -> ST s ()
whtRow coeffs row = do
  i0 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (row * 4 + 0)
  i1 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (row * 4 + 1)
  i2 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (row * 4 + 2)
  i3 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (row * 4 + 3)

  let a = i0 + i3
      b = i1 + i2
      c = i1 - i2
      d = i0 - i3

      -- No scaling in row pass (first pass)
      o0 = a + b
      o1 = c + d
      o2 = a - b
      o3 = d - c

  VSM.unsafeWrite coeffs (row * 4 + 0) (fromIntegral o0)
  VSM.unsafeWrite coeffs (row * 4 + 1) (fromIntegral o1)
  VSM.unsafeWrite coeffs (row * 4 + 2) (fromIntegral o2)
  VSM.unsafeWrite coeffs (row * 4 + 3) (fromIntegral o3)

-- | WHT column transformation (divide by 8 per RFC 6386 - second pass)
{-# INLINE whtColumn #-}
whtColumn :: VSM.MVector s Int16 -> Int -> ST s ()
whtColumn coeffs col = do
  i0 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (0 * 4 + col)
  i1 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (1 * 4 + col)
  i2 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (2 * 4 + col)
  i3 <- (fromIntegral :: Int16 -> Int) <$> VSM.unsafeRead coeffs (3 * 4 + col)

  let a = i0 + i3
      b = i1 + i2
      c = i1 - i2
      d = i0 - i3

      -- Divide by 8 with rounding in column pass (second pass) per RFC 6386
      o0 = (a + b + 3) `shiftR` 3
      o1 = (c + d + 3) `shiftR` 3
      o2 = (a - b + 3) `shiftR` 3
      o3 = (d - c + 3) `shiftR` 3

  VSM.unsafeWrite coeffs (0 * 4 + col) (fromIntegral o0)
  VSM.unsafeWrite coeffs (1 * 4 + col) (fromIntegral o1)
  VSM.unsafeWrite coeffs (2 * 4 + col) (fromIntegral o2)
  VSM.unsafeWrite coeffs (3 * 4 + col) (fromIntegral o3)
