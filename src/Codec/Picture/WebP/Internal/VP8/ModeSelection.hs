{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.ModeSelection
  ( selectIntra16x16Mode,
    selectIntra4x4Mode,
    selectChromaMode,
  )
where

import Codec.Picture.WebP.Internal.VP8.Predict
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Word
import qualified Data.Vector.Storable.Mutable as VSM

-- Prediction modes (from Tables.hs and Predict.hs)
-- 16x16 luma modes: DC_PRED=0, V_PRED=1, H_PRED=2, TM_PRED=3
-- 4x4 luma modes: B_DC_PRED=0, B_TM_PRED=1, B_VE_PRED=2, B_HE_PRED=3, ...
-- 8x8 chroma modes: DC_PRED=0, V_PRED=1, H_PRED=2, TM_PRED=3

-- | Select best 16x16 intra prediction mode using SAD
-- Returns (mode, sad) where mode is 0-3 (DC/V/H/TM)
selectIntra16x16Mode ::
  VSM.MVector s Word8 -> -- Y plane original buffer
  VSM.MVector s Word8 -> -- Y plane reconstruction buffer (for prediction)
  Int -> -- Stride (width of Y plane)
  Int -> -- Macroblock X position (in pixels)
  Int -> -- Macroblock Y position (in pixels)
  ST s (Int, Int) -- (mode, sad)
selectIntra16x16Mode yOrig yRecon stride mbX mbY = do
  -- Allocate temporary prediction buffer
  predBuf <- VSM.clone yRecon

  -- Try all 4 modes and find the one with lowest SAD
  results <- mapM (\mode -> do
    -- Compute prediction for this mode
    predict16x16 mode predBuf stride mbX mbY

    -- Calculate SAD (sum of absolute differences)
    sad <- computeSAD16x16 yOrig predBuf stride mbX mbY

    return (mode, sad)
    ) [0..3]

  -- Return mode with minimum SAD
  return $ foldl1 (\(m1, s1) (m2, s2) -> if s2 < s1 then (m2, s2) else (m1, s1)) results

-- | Select best 4x4 intra prediction mode for a sub-block
-- Returns (mode, sad) where mode is 0-9 (B_DC_PRED through B_HU_PRED)
selectIntra4x4Mode ::
  VSM.MVector s Word8 -> -- Y plane original buffer
  VSM.MVector s Word8 -> -- Y plane reconstruction buffer
  Int -> -- Stride
  Int -> -- Macroblock X (in pixels)
  Int -> -- Macroblock Y (in pixels)
  Int -> -- Sub-block index (0-15)
  ST s (Int, Int)
selectIntra4x4Mode yOrig yRecon stride mbX mbY subBlock = do
  let subX = mbX + (subBlock `mod` 4) * 4
      subY = mbY + (subBlock `div` 4) * 4

  -- Allocate temporary prediction buffer
  predBuf <- VSM.clone yRecon

  -- Try all 10 modes
  results <- mapM (\mode -> do
    predict4x4 mode predBuf stride subX subY
    sad <- computeSAD4x4 yOrig predBuf stride subX subY
    return (mode, sad)
    ) [0..9]

  return $ foldl1 (\(m1, s1) (m2, s2) -> if s2 < s1 then (m2, s2) else (m1, s1)) results

-- | Select best chroma (U/V) 8x8 intra prediction mode
-- Returns (mode, sad) where mode is 0-3 (DC/V/H/TM)
selectChromaMode ::
  VSM.MVector s Word8 -> -- U or V plane original buffer
  VSM.MVector s Word8 -> -- U or V plane reconstruction buffer
  Int -> -- Stride (width of chroma plane)
  Int -> -- Macroblock X (in pixels, chroma coordinates)
  Int -> -- Macroblock Y (in pixels, chroma coordinates)
  ST s (Int, Int)
selectChromaMode chromaOrig chromaRecon stride mbX mbY = do
  predBuf <- VSM.clone chromaRecon

  results <- mapM (\mode -> do
    predict8x8 mode predBuf stride mbX mbY
    sad <- computeSAD8x8 chromaOrig predBuf stride mbX mbY
    return (mode, sad)
    ) [0..3]

  return $ foldl1 (\(m1, s1) (m2, s2) -> if s2 < s1 then (m2, s2) else (m1, s1)) results

-- | Compute Sum of Absolute Differences for 16x16 block
computeSAD16x16 ::
  VSM.MVector s Word8 -> -- Original
  VSM.MVector s Word8 -> -- Prediction
  Int -> -- Stride
  Int -> Int -> -- X, Y position
  ST s Int
computeSAD16x16 orig pred stride x y = do
  let loop !row !col !acc
        | row >= 16 = return acc
        | col >= 16 = loop (row + 1) 0 acc
        | otherwise = do
            let idx = (y + row) * stride + (x + col)
            o <- VSM.read orig idx
            p <- VSM.read pred idx
            let diff = abs (fromIntegral o - fromIntegral p :: Int)
            loop row (col + 1) (acc + diff)
  loop 0 0 0

-- | Compute SAD for 8x8 block
computeSAD8x8 ::
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  Int ->
  Int -> Int ->
  ST s Int
computeSAD8x8 orig pred stride x y = do
  let loop !row !col !acc
        | row >= 8 = return acc
        | col >= 8 = loop (row + 1) 0 acc
        | otherwise = do
            let idx = (y + row) * stride + (x + col)
            o <- VSM.read orig idx
            p <- VSM.read pred idx
            let diff = abs (fromIntegral o - fromIntegral p :: Int)
            loop row (col + 1) (acc + diff)
  loop 0 0 0

-- | Compute SAD for 4x4 block
computeSAD4x4 ::
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  Int ->
  Int -> Int ->
  ST s Int
computeSAD4x4 orig pred stride x y = do
  let loop !row !col !acc
        | row >= 4 = return acc
        | col >= 4 = loop (row + 1) 0 acc
        | otherwise = do
            let idx = (y + row) * stride + (x + col)
            o <- VSM.read orig idx
            p <- VSM.read pred idx
            let diff = abs (fromIntegral o - fromIntegral p :: Int)
            loop row (col + 1) (acc + diff)
  loop 0 0 0
