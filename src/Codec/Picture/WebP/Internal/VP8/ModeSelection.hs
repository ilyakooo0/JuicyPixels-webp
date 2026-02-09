{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.ModeSelection
  ( selectIntra16x16Mode,
    selectIntra4x4Mode,
    selectChromaMode,
  )
where

import Codec.Picture.WebP.Internal.VP8.Predict
import Control.Monad.ST
import Data.Bits (shiftR, (.&.))
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word

-- Prediction modes (from Tables.hs and Predict.hs)
-- 16x16 luma modes: DC_PRED=0, V_PRED=1, H_PRED=2, TM_PRED=3
-- 4x4 luma modes: B_DC_PRED=0, B_TM_PRED=1, B_VE_PRED=2, B_HE_PRED=3, ...
-- 8x8 chroma modes: DC_PRED=0, V_PRED=1, H_PRED=2, TM_PRED=3

-- | Early exit threshold - if SAD is below this, stop searching
-- This represents a "good enough" match that's unlikely to be beaten
earlyExitThreshold16x16 :: Int
earlyExitThreshold16x16 = 128 -- ~0.5 per pixel on average
{-# INLINE earlyExitThreshold16x16 #-}

earlyExitThreshold8x8 :: Int
earlyExitThreshold8x8 = 32 -- ~0.5 per pixel on average
{-# INLINE earlyExitThreshold8x8 #-}

earlyExitThreshold4x4 :: Int
earlyExitThreshold4x4 = 8 -- ~0.5 per pixel on average
{-# INLINE earlyExitThreshold4x4 #-}

-- | Select best 16x16 intra prediction mode using SAD
-- Returns (mode, sad) where mode is 0-3 (DC/V/H/TM)
-- Optimized: single prediction buffer, early exit, incremental SAD
{-# INLINE selectIntra16x16Mode #-}
selectIntra16x16Mode ::
  VSM.MVector s Word8 -> -- Y plane original buffer
  VSM.MVector s Word8 -> -- Y plane reconstruction buffer (for prediction)
  Int -> -- Stride (width of Y plane)
  Int -> -- Macroblock X position (in pixels)
  Int -> -- Macroblock Y position (in pixels)
  ST s (Int, Int) -- (mode, sad)
selectIntra16x16Mode yOrig yRecon stride mbX mbY = do
  -- Allocate single temporary prediction buffer (reused for all modes)
  predBuf <- VSM.clone yRecon

  -- Try modes in order of typical efficiency: DC, V, H, TM
  -- Start with DC (mode 0) as baseline
  predict16x16 0 predBuf stride mbX mbY
  !sad0 <- computeSAD16x16Fast yOrig predBuf stride mbX mbY

  -- Early exit if DC mode is excellent
  if sad0 < earlyExitThreshold16x16
    then return (0, sad0)
    else do
      -- Try V_PRED (mode 1)
      predict16x16 1 predBuf stride mbX mbY
      !sad1 <- computeSAD16x16Fast yOrig predBuf stride mbX mbY

      let (!best1, !bestSad1) = if sad1 < sad0 then (1, sad1) else (0, sad0)

      if bestSad1 < earlyExitThreshold16x16
        then return (best1, bestSad1)
        else do
          -- Try H_PRED (mode 2)
          predict16x16 2 predBuf stride mbX mbY
          !sad2 <- computeSAD16x16Fast yOrig predBuf stride mbX mbY

          let (!best2, !bestSad2) = if sad2 < bestSad1 then (2, sad2) else (best1, bestSad1)

          if bestSad2 < earlyExitThreshold16x16
            then return (best2, bestSad2)
            else do
              -- Try TM_PRED (mode 3)
              predict16x16 3 predBuf stride mbX mbY
              !sad3 <- computeSAD16x16Fast yOrig predBuf stride mbX mbY

              let (!best3, !bestSad3) = if sad3 < bestSad2 then (3, sad3) else (best2, bestSad2)
              return (best3, bestSad3)

-- | Select best 4x4 intra prediction mode for a sub-block
-- Returns (mode, sad) where mode is 0-9 (B_DC_PRED through B_HU_PRED)
-- Optimized: single buffer, early exit, strict loop
{-# INLINE selectIntra4x4Mode #-}
selectIntra4x4Mode ::
  VSM.MVector s Word8 -> -- Y plane original buffer
  VSM.MVector s Word8 -> -- Y plane reconstruction buffer
  Int -> -- Stride
  Int -> -- Macroblock X (in pixels)
  Int -> -- Macroblock Y (in pixels)
  Int -> -- Sub-block index (0-15)
  ST s (Int, Int)
selectIntra4x4Mode yOrig yRecon stride mbX mbY subBlock = do
  let !subX = mbX + (subBlock .&. 3) * 4 -- subBlock `mod` 4 as bit op
      !subY = mbY + (subBlock `shiftR` 2) * 4 -- subBlock `div` 4 as shift

  -- Allocate single temporary prediction buffer
  predBuf <- VSM.clone yRecon

  -- Try all 10 modes with early exit
  let go !mode !bestMode !bestSAD
        | mode > 9 = return (bestMode, bestSAD)
        | bestSAD == 0 = return (bestMode, bestSAD) -- Can't do better than 0
        | otherwise = do
            predict4x4 mode predBuf stride subX subY
            !sad <- computeSAD4x4Fast yOrig predBuf stride subX subY
            if sad < bestSAD
              then
                if sad < earlyExitThreshold4x4
                  then return (mode, sad) -- Early exit
                  else go (mode + 1) mode sad
              else go (mode + 1) bestMode bestSAD

  -- Start with mode 0
  predict4x4 0 predBuf stride subX subY
  !sad0 <- computeSAD4x4Fast yOrig predBuf stride subX subY
  if sad0 < earlyExitThreshold4x4
    then return (0, sad0)
    else go 1 0 sad0

-- | Select best chroma (U/V) 8x8 intra prediction mode
-- Returns (mode, sad) where mode is 0-3 (DC/V/H/TM)
-- Optimized: single buffer, early exit
{-# INLINE selectChromaMode #-}
selectChromaMode ::
  VSM.MVector s Word8 -> -- U or V plane original buffer
  VSM.MVector s Word8 -> -- U or V plane reconstruction buffer
  Int -> -- Stride (width of chroma plane)
  Int -> -- Macroblock X (in pixels, chroma coordinates)
  Int -> -- Macroblock Y (in pixels, chroma coordinates)
  ST s (Int, Int)
selectChromaMode chromaOrig chromaRecon stride mbX mbY = do
  predBuf <- VSM.clone chromaRecon

  -- Try DC (mode 0) first
  predict8x8 0 predBuf stride mbX mbY
  !sad0 <- computeSAD8x8Fast chromaOrig predBuf stride mbX mbY

  if sad0 < earlyExitThreshold8x8
    then return (0, sad0)
    else do
      -- Try V_PRED (mode 1)
      predict8x8 1 predBuf stride mbX mbY
      !sad1 <- computeSAD8x8Fast chromaOrig predBuf stride mbX mbY

      let (!best1, !bestSad1) = if sad1 < sad0 then (1, sad1) else (0, sad0)

      if bestSad1 < earlyExitThreshold8x8
        then return (best1, bestSad1)
        else do
          -- Try H_PRED (mode 2)
          predict8x8 2 predBuf stride mbX mbY
          !sad2 <- computeSAD8x8Fast chromaOrig predBuf stride mbX mbY

          let (!best2, !bestSad2) = if sad2 < bestSad1 then (2, sad2) else (best1, bestSad1)

          if bestSad2 < earlyExitThreshold8x8
            then return (best2, bestSad2)
            else do
              -- Try TM_PRED (mode 3)
              predict8x8 3 predBuf stride mbX mbY
              !sad3 <- computeSAD8x8Fast chromaOrig predBuf stride mbX mbY

              let (!best3, !bestSad3) = if sad3 < bestSad2 then (3, sad3) else (best2, bestSad2)
              return (best3, bestSad3)

-- | Compute Sum of Absolute Differences for 16x16 block
-- Optimized with pre-computed row bases, unsafeRead, and strict accumulation
{-# INLINE computeSAD16x16Fast #-}
computeSAD16x16Fast ::
  VSM.MVector s Word8 -> -- Original
  VSM.MVector s Word8 -> -- Prediction
  Int -> -- Stride
  Int ->
  Int -> -- X, Y position
  ST s Int
computeSAD16x16Fast orig pred_ stride x y = do
  let !baseY = y * stride + x

  -- Unroll outer loop for better performance
  let goRow !row !acc
        | row >= 16 = return acc
        | otherwise = do
            let !rowBase = baseY + row * stride
            -- Process 16 columns with manual unrolling (4 at a time)
            !acc0 <- goCol4 rowBase 0 acc
            !acc1 <- goCol4 rowBase 4 acc0
            !acc2 <- goCol4 rowBase 8 acc1
            !acc3 <- goCol4 rowBase 12 acc2
            goRow (row + 1) acc3

      goCol4 !rowBase !col !acc = do
        let !idx0 = rowBase + col
            !idx1 = idx0 + 1
            !idx2 = idx0 + 2
            !idx3 = idx0 + 3
        !o0 <- VSM.unsafeRead orig idx0
        !p0 <- VSM.unsafeRead pred_ idx0
        !o1 <- VSM.unsafeRead orig idx1
        !p1 <- VSM.unsafeRead pred_ idx1
        !o2 <- VSM.unsafeRead orig idx2
        !p2 <- VSM.unsafeRead pred_ idx2
        !o3 <- VSM.unsafeRead orig idx3
        !p3 <- VSM.unsafeRead pred_ idx3
        let !d0 = abs (fromIntegral o0 - fromIntegral p0 :: Int)
            !d1 = abs (fromIntegral o1 - fromIntegral p1 :: Int)
            !d2 = abs (fromIntegral o2 - fromIntegral p2 :: Int)
            !d3 = abs (fromIntegral o3 - fromIntegral p3 :: Int)
        return $! acc + d0 + d1 + d2 + d3

  goRow 0 0

-- | Compute SAD for 8x8 block - optimized version
{-# INLINE computeSAD8x8Fast #-}
computeSAD8x8Fast ::
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  ST s Int
computeSAD8x8Fast orig pred_ stride x y = do
  let !baseY = y * stride + x

  let goRow !row !acc
        | row >= 8 = return acc
        | otherwise = do
            let !rowBase = baseY + row * stride
            -- Process 8 columns with manual unrolling (4 at a time)
            !acc0 <- goCol4 rowBase 0 acc
            !acc1 <- goCol4 rowBase 4 acc0
            goRow (row + 1) acc1

      goCol4 !rowBase !col !acc = do
        let !idx0 = rowBase + col
            !idx1 = idx0 + 1
            !idx2 = idx0 + 2
            !idx3 = idx0 + 3
        !o0 <- VSM.unsafeRead orig idx0
        !p0 <- VSM.unsafeRead pred_ idx0
        !o1 <- VSM.unsafeRead orig idx1
        !p1 <- VSM.unsafeRead pred_ idx1
        !o2 <- VSM.unsafeRead orig idx2
        !p2 <- VSM.unsafeRead pred_ idx2
        !o3 <- VSM.unsafeRead orig idx3
        !p3 <- VSM.unsafeRead pred_ idx3
        let !d0 = abs (fromIntegral o0 - fromIntegral p0 :: Int)
            !d1 = abs (fromIntegral o1 - fromIntegral p1 :: Int)
            !d2 = abs (fromIntegral o2 - fromIntegral p2 :: Int)
            !d3 = abs (fromIntegral o3 - fromIntegral p3 :: Int)
        return $! acc + d0 + d1 + d2 + d3

  goRow 0 0

-- | Compute SAD for 4x4 block - optimized version
{-# INLINE computeSAD4x4Fast #-}
computeSAD4x4Fast ::
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  ST s Int
computeSAD4x4Fast orig pred_ stride x y = do
  let !baseY = y * stride + x

  let goRow !row !acc
        | row >= 4 = return acc
        | otherwise = do
            let !rowBase = baseY + row * stride
            -- Process all 4 columns at once
            let !idx0 = rowBase
                !idx1 = rowBase + 1
                !idx2 = rowBase + 2
                !idx3 = rowBase + 3
            !o0 <- VSM.unsafeRead orig idx0
            !p0 <- VSM.unsafeRead pred_ idx0
            !o1 <- VSM.unsafeRead orig idx1
            !p1 <- VSM.unsafeRead pred_ idx1
            !o2 <- VSM.unsafeRead orig idx2
            !p2 <- VSM.unsafeRead pred_ idx2
            !o3 <- VSM.unsafeRead orig idx3
            !p3 <- VSM.unsafeRead pred_ idx3
            let !d0 = abs (fromIntegral o0 - fromIntegral p0 :: Int)
                !d1 = abs (fromIntegral o1 - fromIntegral p1 :: Int)
                !d2 = abs (fromIntegral o2 - fromIntegral p2 :: Int)
                !d3 = abs (fromIntegral o3 - fromIntegral p3 :: Int)
            goRow (row + 1) (acc + d0 + d1 + d2 + d3)

  goRow 0 0
