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

-- Performance: INLINE pragmas on all transform functions

-- Forward DCT constants (from libwebp's FTransform)
-- These differ from inverse DCT constants for numerical precision
kC1 :: Int
kC1 = 20091 -- cos(pi/8) * sqrt(2) * 65536 - 65536

kC2 :: Int
kC2 = 35468 -- sin(pi/8) * sqrt(2) * 65536

-- libwebp forward DCT constants (smaller scale, different bias)
fdctC1 :: Int
fdctC1 = 2217 -- cos(pi/8) * sqrt(2) * 2048 - 2048 ≈ 2217

fdctC2 :: Int
fdctC2 = 5352 -- sin(pi/8) * sqrt(2) * 8192 ≈ 5352

-- | 4x4 forward DCT (in-place)
-- Input: spatial domain residuals (original - prediction)
-- Output: frequency domain coefficients
-- Row pass first, then column pass (opposite order from inverse)
{-# INLINE fdct4x4 #-}
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

-- | Forward DCT row transformation (based on libwebp's FTransform)
{-# INLINE fdctRow #-}
fdctRow :: VSM.MVector s Int16 -> Int -> ST s ()
fdctRow residuals row = do
  d0 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (row * 4 + 0)
  d1 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (row * 4 + 1)
  d2 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (row * 4 + 2)
  d3 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (row * 4 + 3)

  -- Forward butterfly
  let a0 = d0 + d3
      a1 = d1 + d2
      a2 = d1 - d2
      a3 = d0 - d3

  -- libwebp's forward DCT row formulas
  -- DC and AC2 scaled by 8, AC1 and AC3 use rotated basis
  let o0 = (a0 + a1) * 8
      o2 = (a0 - a1) * 8
      o1 = (a2 * fdctC1 + a3 * fdctC2 + 1812) `shiftR` 9
      o3 = (a3 * fdctC1 - a2 * fdctC2 + 937) `shiftR` 9

  VSM.write residuals (row * 4 + 0) (fromIntegral o0)
  VSM.write residuals (row * 4 + 1) (fromIntegral o1)
  VSM.write residuals (row * 4 + 2) (fromIntegral o2)
  VSM.write residuals (row * 4 + 3) (fromIntegral o3)

-- | Forward DCT column transformation (based on libwebp's FTransform)
{-# INLINE fdctColumn #-}
fdctColumn :: VSM.MVector s Int16 -> Int -> ST s ()
fdctColumn residuals col = do
  t0 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (0 * 4 + col)
  t4 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (1 * 4 + col)
  t8 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (2 * 4 + col)
  t12 <- (fromIntegral :: Int16 -> Int) <$> VSM.read residuals (3 * 4 + col)

  -- Forward butterfly
  let a0 = t0 + t12
      a1 = t4 + t8
      a2 = t4 - t8
      a3 = t0 - t12

  -- libwebp's forward DCT column formulas
  -- DC and AC2 divided by 16 (row was *8, so net /2 for DC)
  let o0 = (a0 + a1 + 7) `shiftR` 4
      o2 = (a0 - a1 + 7) `shiftR` 4
      -- AC1 and AC3 with rotated basis and bias, divided by 65536
      o1 = ((a2 * fdctC1 + a3 * fdctC2 + 12000) `shiftR` 16) + (if a3 /= 0 then 1 else 0)
      o3 = ((a3 * fdctC1 - a2 * fdctC2 + 51000) `shiftR` 16) - (if a2 /= 0 then 1 else 0)

  VSM.write residuals (0 * 4 + col) (fromIntegral o0)
  VSM.write residuals (1 * 4 + col) (fromIntegral o1)
  VSM.write residuals (2 * 4 + col) (fromIntegral o2)
  VSM.write residuals (3 * 4 + col) (fromIntegral o3)

-- | Forward Walsh-Hadamard Transform for Y2 DC block
-- Input: 16 DC values from the 16 Y blocks (in 4x4 arrangement)
-- Output: Transformed Y2 coefficients
-- Based on libwebp's FTransformWHT: columns first (scale by 1/2), then rows (no scale)
-- This is the transpose of the inverse (which does rows then columns)
{-# INLINE fwht4x4 #-}
fwht4x4 :: VSM.MVector s Int16 -> ST s ()
fwht4x4 dcs = do
  -- Forward WHT: columns first (with 1/2 scale), then rows
  fwhtColumn dcs 0
  fwhtColumn dcs 1
  fwhtColumn dcs 2
  fwhtColumn dcs 3

  fwhtRow dcs 0
  fwhtRow dcs 1
  fwhtRow dcs 2
  fwhtRow dcs 3

-- | Forward WHT column transformation (with 1/2 scaling)
-- First pass - matches libwebp's FTransformWHT column loop
{-# INLINE fwhtColumn #-}
fwhtColumn :: VSM.MVector s Int16 -> Int -> ST s ()
fwhtColumn dcs col = do
  i0 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (0 * 4 + col)
  i1 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (1 * 4 + col)
  i2 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (2 * 4 + col)
  i3 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (3 * 4 + col)

  -- Same butterfly pattern as inverse WHT
  let a = i0 + i3
      b = i1 + i2
      c = i1 - i2
      d = i0 - i3

      -- Divide by 2 (scale factor for forward transform)
      o0 = (a + b) `shiftR` 1
      o1 = (c + d) `shiftR` 1
      o2 = (a - b) `shiftR` 1
      o3 = (d - c) `shiftR` 1

  VSM.write dcs (0 * 4 + col) (fromIntegral o0)
  VSM.write dcs (1 * 4 + col) (fromIntegral o1)
  VSM.write dcs (2 * 4 + col) (fromIntegral o2)
  VSM.write dcs (3 * 4 + col) (fromIntegral o3)

-- | Forward WHT row transformation (no scaling)
-- Second pass - matches libwebp's FTransformWHT row loop
{-# INLINE fwhtRow #-}
fwhtRow :: VSM.MVector s Int16 -> Int -> ST s ()
fwhtRow dcs row = do
  i0 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (row * 4 + 0)
  i1 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (row * 4 + 1)
  i2 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (row * 4 + 2)
  i3 <- (fromIntegral :: Int16 -> Int) <$> VSM.read dcs (row * 4 + 3)

  -- Same butterfly pattern as inverse WHT
  let a = i0 + i3
      b = i1 + i2
      c = i1 - i2
      d = i0 - i3

      -- No scaling in row pass
      o0 = a + b
      o1 = c + d
      o2 = a - b
      o3 = d - c

  VSM.write dcs (row * 4 + 0) (fromIntegral o0)
  VSM.write dcs (row * 4 + 1) (fromIntegral o1)
  VSM.write dcs (row * 4 + 2) (fromIntegral o2)
  VSM.write dcs (row * 4 + 3) (fromIntegral o3)
