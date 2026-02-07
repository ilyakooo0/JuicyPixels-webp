{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.ModeSelection
  ( selectIntra16x16Mode,
    selectIntra4x4Mode,
    selectChromaMode,
  )
where

import Control.Monad (forM_)
import Control.Monad.ST
import Data.Word
import qualified Data.Vector.Storable.Mutable as VSM

-- Prediction modes (from Tables.hs and Predict.hs)
-- 16x16 luma modes: DC_PRED=0, V_PRED=1, H_PRED=2, TM_PRED=3
-- 4x4 luma modes: B_DC_PRED=0, B_TM_PRED=1, B_VE_PRED=2, B_HE_PRED=3, ...
-- 8x8 chroma modes: DC_PRED=0, V_PRED=1, H_PRED=2, TM_PRED=3

-- | Select best 16x16 intra prediction mode
-- Returns (mode, sad) where mode is 0-3 (DC/V/H/TM)
-- For simplicity, always returns DC_PRED (mode 0) which works well for most content
selectIntra16x16Mode ::
  VSM.MVector s Word8 -> -- Y plane buffer
  Int -> -- Stride (width of Y plane)
  Int -> -- Macroblock X position (in macroblocks)
  Int -> -- Macroblock Y position (in macroblocks)
  ST s (Int, Int) -- (mode, sad)
selectIntra16x16Mode _yBuf _stride _mbX _mbY = do
  -- Simplified: always use DC_PRED (mode 0)
  -- A full implementation would:
  -- 1. Try all 4 modes (DC, V, H, TM)
  -- 2. Compute prediction for each mode
  -- 3. Calculate SAD (sum of absolute differences) vs original
  -- 4. Return mode with lowest SAD
  return (0, 0)

-- | Select best 4x4 intra prediction mode for a sub-block
-- Returns (mode, sad) where mode is 0-9 (B_DC_PRED through B_RD_PRED)
-- For simplicity, always returns B_DC_PRED (mode 0)
selectIntra4x4Mode ::
  VSM.MVector s Word8 -> -- Y plane buffer
  Int -> -- Stride
  Int -> -- Macroblock X
  Int -> -- Macroblock Y
  Int -> -- Sub-block index (0-15)
  ST s (Int, Int)
selectIntra4x4Mode _yBuf _stride _mbX _mbY _subBlock = do
  -- Simplified: always use B_DC_PRED (mode 0)
  return (0, 0)

-- | Select best chroma (U/V) 8x8 intra prediction mode
-- Returns (mode, sad) where mode is 0-3 (DC/V/H/TM)
-- For simplicity, always returns DC_PRED (mode 0)
selectChromaMode ::
  VSM.MVector s Word8 -> -- U or V plane buffer
  Int -> -- Stride (width of chroma plane)
  Int -> -- Macroblock X
  Int -> -- Macroblock Y
  ST s (Int, Int)
selectChromaMode _uBuf _stride _mbX _mbY = do
  -- Simplified: always use DC_PRED (mode 0)
  return (0, 0)
