{-# LANGUAGE BangPatterns #-}

-- | Adaptive loop filter strength optimization for the VP8 encoder.
-- Searches for the filter level that minimizes SSE against the source image.
module Codec.Picture.WebP.Internal.VP8.FilterStrengthSearch
  ( optimizeFilterStrength,
    optimizeFilterStrengthPerSegment,
  )
where

import Codec.Picture.WebP.Internal.VP8.LoopFilter (applyNormalLoopFilterRow, applyNormalLoopFilterRowSegmented)
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Int (Int64)
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
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
  Maybe (VU.Vector Int, VU.Vector Word8) ->
  -- | per-segment filter info: (filter deltas, segment map)
  ST s Int
optimizeFilterStrength ySrc uSrc vSrc yPF uPF vPF paddedW mbRows mbCols defaultLevel mSegFilterInfo = do
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
              case mSegFilterInfo of
                Just (segFD, segMap) ->
                  applyNormalLoopFilterRowSegmented yScratch paddedW uScratch uvStride vScratch uvStride mbRow mbCols level segFD segMap
                Nothing ->
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

-- | Find optimal per-segment filter levels that minimize per-segment distortion.
-- For each segment, independently searches for the filter level that minimizes
-- that segment's SSE against the source image.
-- Returns (base filter level, per-segment filter deltas).
optimizeFilterStrengthPerSegment ::
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
  -- | default filter level (qi/2)
  VU.Vector Int ->
  -- | QP-derived per-segment filter deltas
  VU.Vector Word8 ->
  -- | segment map (mbRows × mbCols, segment ID per MB)
  ST s (Int, VU.Vector Int)
optimizeFilterStrengthPerSegment ySrc uSrc vSrc yPF uPF vPF paddedW mbRows mbCols defaultLevel qpFilterDeltas segMap = do
  let !uvStride = paddedW `div` 2
      !numSegs = VU.length qpFilterDeltas

  -- Initial absolute levels from QP-derived deltas
  let !initLevels = VU.generate numSegs $ \s ->
        max 0 $ min 63 $ defaultLevel + (qpFilterDeltas VU.! s)

  -- Per-segment no-filter baseline SSE (comparing source with unfiltered reconstruction)
  noFilterSSEs <- VU.generateM numSegs $ \seg ->
    computeSegmentSSE ySrc uSrc vSrc yPF uPF vPF paddedW mbCols seg segMap

  -- Scratch buffers (reused for each trial)
  yScratch <- VSM.new (VSM.length yPF)
  uScratch <- VSM.new (VSM.length uPF)
  vScratch <- VSM.new (VSM.length vPF)

  -- Mutable best levels per segment
  bestLevels <- VUM.new numSegs
  forM_ [0 .. numSegs - 1] $ \s ->
    VUM.write bestLevels s (initLevels VU.! s)

  -- Greedy per-segment optimization: for each segment, find the filter level
  -- minimizing that segment's SSE while holding other segments at current best.
  forM_ [0 .. numSegs - 1] $ \seg -> do
    let !segCount = VU.foldl' (\acc s -> if fromIntegral s == (seg :: Int) then acc + 1 else acc) (0 :: Int) segMap
    if segCount == 0
      then VUM.write bestLevels seg 0
      else do
        let !center = initLevels VU.! seg
            !lo = max 1 (center - 10)
            !hi = min 63 (center + 10)
            !segSSE0 = noFilterSSEs VU.! seg

        -- Freeze current levels for other segments (stable during this segment's search)
        curLevels <- VU.freeze bestLevels

        let search !level !bestL !bestSSE
              | level > hi = VUM.write bestLevels seg bestL
              | otherwise = do
                  -- Trial: set this segment to candidate level, keep others at current best
                  -- Using base=0 with absolute levels as "deltas"
                  let !trialLevels = curLevels VU.// [(seg, level)]

                  VSM.copy yScratch yPF
                  VSM.copy uScratch uPF
                  VSM.copy vScratch vPF
                  forM_ [0 .. mbRows - 1] $ \mbRow ->
                    applyNormalLoopFilterRowSegmented yScratch paddedW uScratch uvStride vScratch uvStride mbRow mbCols 0 trialLevels segMap

                  sse <- computeSegmentSSE ySrc uSrc vSrc yScratch uScratch vScratch paddedW mbCols seg segMap

                  if sse < bestSSE
                    then search (level + 1) level sse
                    else search (level + 1) bestL bestSSE

        search lo 0 segSSE0

  -- Derive base level + deltas from per-segment absolute levels
  finalLevels <- VU.freeze bestLevels
  let !sumLevels = VU.foldl' (+) 0 finalLevels
      !anyNonZero = VU.any (> 0) finalLevels
      -- Ensure base > 0 when any segment needs filtering, so the
      -- `when (filterLevel > 0)` guard in the encoder doesn't skip everything.
      !baseLevel =
        if anyNonZero
          then max 1 $ min 63 $ (sumLevels + numSegs `div` 2) `div` numSegs
          else 0
      !deltas = VU.map (\l -> max (-63) $ min 63 $ l - baseLevel) finalLevels

  return (baseLevel, deltas)

-- | Compute SSE for macroblocks belonging to a specific segment.
computeSegmentSSE ::
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  -- | Y/U/V source
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  -- | Y/U/V reconstruction
  Int ->
  -- | padded width (Y stride)
  Int ->
  -- | MB cols
  Int ->
  -- | target segment ID
  VU.Vector Word8 ->
  -- | segment map
  ST s Int64
computeSegmentSSE ySrc uSrc vSrc yRec uRec vRec paddedW mbCols seg segMap = do
  let !uvStride = paddedW `div` 2
      !totalMBs = VU.length segMap
      go !mbIdx !acc
        | mbIdx >= totalMBs = return acc
        | fromIntegral (segMap VU.! mbIdx) /= seg = go (mbIdx + 1) acc
        | otherwise = do
            let !mbX = mbIdx `mod` mbCols
                !mbY = mbIdx `div` mbCols
            !ySSE <- computeBlockSSE ySrc yRec paddedW (mbX * 16) (mbY * 16) 16
            !uSSE <- computeBlockSSE uSrc uRec uvStride (mbX * 8) (mbY * 8) 8
            !vSSE <- computeBlockSSE vSrc vRec uvStride (mbX * 8) (mbY * 8) 8
            go (mbIdx + 1) (acc + ySSE + uSSE + vSSE)
  go 0 0

-- | SSE between two planes for a square block at position (bx, by).
{-# INLINE computeBlockSSE #-}
computeBlockSSE ::
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  Int ->
  ST s Int64
computeBlockSSE src rec stride bx by bSize = do
  let go !row !acc
        | row >= bSize = return acc
        | otherwise = do
            let !base = (by + row) * stride + bx
            !rowSSE <- goCol base 0 0
            go (row + 1) (acc + rowSSE)
      goCol !base !col !acc
        | col >= bSize = return acc
        | otherwise = do
            !s <- VSM.unsafeRead src (base + col)
            !r <- VSM.unsafeRead rec (base + col)
            let !d = fromIntegral s - fromIntegral r :: Int
            goCol base (col + 1) (acc + fromIntegral (d * d))
  go 0 0
