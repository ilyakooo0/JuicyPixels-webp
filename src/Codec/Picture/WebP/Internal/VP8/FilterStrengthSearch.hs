{-# LANGUAGE BangPatterns #-}

-- | Adaptive loop filter strength optimization for the VP8 encoder.
-- Searches for the filter level that minimizes SSE against the source image.
module Codec.Picture.WebP.Internal.VP8.FilterStrengthSearch
  ( optimizeFilterStrength,
  )
where

import Codec.Picture.WebP.Internal.VP8.LoopFilter (applyNormalLoopFilterRow)
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Int (Int64)
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word

-- | Find the filter level (0-63) that minimizes distortion against the source.
-- Tests level 0 (no filter) and a range around the default level.
optimizeFilterStrength ::
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  -- | Y/U/V source (original pixels)
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  -- | Y/U/V pre-filter reconstruction
  Int ->
  -- | padded width
  Int ->
  -- | MB rows
  Int ->
  -- | MB cols
  Int ->
  -- | default filter level
  ST s Int
optimizeFilterStrength ySrc uSrc vSrc yPF uPF vPF paddedW mbRows mbCols defaultLevel = do
  -- SSE at level 0 (no filter) — baseline
  sse0 <- computeFrameSSE ySrc uSrc vSrc yPF uPF vPF

  -- Scratch buffers (reused for each trial)
  yScratch <- VSM.new (VSM.length yPF)
  uScratch <- VSM.new (VSM.length uPF)
  vScratch <- VSM.new (VSM.length vPF)

  let !uvStride = paddedW `div` 2
      !lo = max 1 (defaultLevel - 10)
      !hi = min 63 (defaultLevel + 10)

  -- Try each level, keep the one with minimum SSE
  let search !level !bestLevel !bestSSE
        | level > hi = return bestLevel
        | otherwise = do
            VSM.copy yScratch yPF
            VSM.copy uScratch uPF
            VSM.copy vScratch vPF
            forM_ [0 .. mbRows - 1] $ \mbRow ->
              applyNormalLoopFilterRow yScratch paddedW uScratch uvStride vScratch uvStride mbRow mbCols level
            sse <- computeFrameSSE ySrc uSrc vSrc yScratch uScratch vScratch
            if sse < bestSSE
              then search (level + 1) level sse
              else search (level + 1) bestLevel bestSSE

  search lo 0 sse0

-- | Total SSE across Y, U, V planes.
computeFrameSSE ::
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  ST s Int64
computeFrameSSE ySrc uSrc vSrc yRec uRec vRec = do
  !ySSE <- computePlaneSSE ySrc yRec
  !uSSE <- computePlaneSSE uSrc uRec
  !vSSE <- computePlaneSSE vSrc vRec
  return (ySSE + uSSE + vSSE)

-- | SSE between two planes of equal length.
{-# INLINE computePlaneSSE #-}
computePlaneSSE :: VSM.MVector s Word8 -> VSM.MVector s Word8 -> ST s Int64
computePlaneSSE src rec = do
  let !n = VSM.length src
      go !i !acc
        | i >= n = return acc
        | otherwise = do
            !s <- VSM.unsafeRead src i
            !r <- VSM.unsafeRead rec i
            let !d = fromIntegral s - fromIntegral r :: Int
            go (i + 1) (acc + fromIntegral (d * d))
  go 0 0
