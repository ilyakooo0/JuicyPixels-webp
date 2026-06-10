{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.LoopFilter
  ( applyLoopFilterFrame,
    applyNormalLoopFilterRow,
    applyNormalLoopFilterRowSegmented,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- ==========================================================================
-- Whole-frame loop filter (decoder)
-- ==========================================================================

-- | Apply the loop filter to a fully reconstructed frame (RFC 6386 §15).
--
-- Macroblocks are processed in raster order. For each MB with a nonzero
-- filter level: left MB edge, interior vertical edges, top MB edge,
-- interior horizontal edges. Interior (subblock) edges are filtered only
-- when the MB's @filterInner@ flag is set (i.e. the MB has nonzero
-- coefficients or uses B_PRED).
--
-- The simple filter (filterType == 1) applies to the Y plane only; the
-- normal filter applies to Y, U and V.
applyLoopFilterFrame ::
  -- | Filter type (1 = simple, otherwise normal)
  Int ->
  -- | Sharpness level (0-7)
  Int ->
  -- | Per-MB filter level (0-63), raster order; 0 = MB not filtered
  VU.Vector Int ->
  -- | Per-MB filterInner flag, raster order
  VU.Vector Bool ->
  -- | Y plane
  VSM.MVector s Word8 ->
  -- | Y stride
  Int ->
  -- | U plane
  VSM.MVector s Word8 ->
  -- | U stride
  Int ->
  -- | V plane
  VSM.MVector s Word8 ->
  -- | V stride
  Int ->
  -- | MB rows
  Int ->
  -- | MB cols
  Int ->
  ST s ()
applyLoopFilterFrame filterType sharpness levels inners yPlane yStride uPlane uStride vPlane vStride mbRows mbCols = do
  let !yLen = VSM.length yPlane
      !uLen = VSM.length uPlane
      !vLen = VSM.length vPlane
  forM_ [0 .. mbRows - 1] $ \mbY ->
    forM_ [0 .. mbCols - 1] $ \mbX -> do
      let !idx = mbY * mbCols + mbX
          !level = levels VU.! idx
          !inner = inners VU.! idx
      when (level > 0) $ do
        let !iLimit = interiorLimitFor sharpness level
            !mbELimit = (level + 2) * 2 + iLimit
            !subELimit = level * 2 + iLimit
            !hevT = hevThresholdFor level
        if filterType == 1
          then do
            -- Simple filter: luma only, MB and subblock edges
            when (mbX > 0) $
              filterSimpleVEdgeFast yPlane yStride yLen (mbX * 16, mbY * 16) mbELimit
            when inner $
              forM_ [4, 8, 12] $ \dx ->
                filterSimpleVEdgeFast yPlane yStride yLen (mbX * 16 + dx, mbY * 16) subELimit
            when (mbY > 0) $
              filterSimpleHEdgeFast yPlane yStride yLen (mbX * 16, mbY * 16) mbELimit
            when inner $
              forM_ [4, 8, 12] $ \dy ->
                filterSimpleHEdgeFast yPlane yStride yLen (mbX * 16, mbY * 16 + dy) subELimit
          else do
            filterNormalMBPlane yPlane yStride yLen mbX mbY 16 mbELimit subELimit iLimit hevT inner
            filterNormalMBPlane uPlane uStride uLen mbX mbY 8 mbELimit subELimit iLimit hevT inner
            filterNormalMBPlane vPlane vStride vLen mbX mbY 8 mbELimit subELimit iLimit hevT inner

-- | Interior limit derived from the filter level and sharpness (RFC 6386 §15.2).
{-# INLINE interiorLimitFor #-}
interiorLimitFor :: Int -> Int -> Int
interiorLimitFor sharpness level =
  let !il =
        if sharpness > 0
          then min (level `shiftR` (if sharpness > 4 then 2 else 1)) (9 - sharpness)
          else level
   in max 1 il

-- | High-edge-variance threshold for key frames (RFC 6386 §15.2).
{-# INLINE hevThresholdFor #-}
hevThresholdFor :: Int -> Int
hevThresholdFor level
  | level >= 40 = 2
  | level >= 15 = 1
  | otherwise = 0

-- | Normal-filter one MB of one plane: left MB edge, interior vertical
-- edges (if inner), top MB edge, interior horizontal edges (if inner).
filterNormalMBPlane ::
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Bool ->
  ST s ()
filterNormalMBPlane plane stride planeLen mbX mbY blockSize mbELimit subELimit iLimit hevT inner = do
  let !bx = mbX * blockSize
      !by = mbY * blockSize
  -- Vertical MB edge
  when (mbX > 0) $
    normalVMBFast plane stride planeLen bx by blockSize mbELimit iLimit hevT
  -- Vertical sub-block edges (every 4 pixels within MB)
  when inner $
    forM_ [4, 8 .. blockSize - 1] $ \dx ->
      normalVSubFast plane stride planeLen (bx + dx) by blockSize subELimit iLimit hevT
  -- Horizontal MB edge
  when (mbY > 0) $
    normalHMBFast plane stride planeLen bx by blockSize mbELimit iLimit hevT
  -- Horizontal sub-block edges (every 4 pixels within MB)
  when inner $
    forM_ [4, 8 .. blockSize - 1] $ \dy ->
      normalHSubFast plane stride planeLen bx (by + dy) blockSize subELimit iLimit hevT

-- ==========================================================================
-- Simple filter primitives
-- ==========================================================================

-- | Fast vertical edge simple filter (filters 16 rows at column x).
{-# INLINE filterSimpleVEdgeFast #-}
filterSimpleVEdgeFast :: VSM.MVector s Word8 -> Int -> Int -> (Int, Int) -> Int -> ST s ()
filterSimpleVEdgeFast plane stride planeLen (x, y) limit = do
  forM_ [0 .. 15] $ \i -> do
    let !py = y + i
        !baseIdx = py * stride + x
    -- Check if all indices are in bounds
    when (baseIdx - 2 >= 0 && baseIdx + 1 < planeLen) $ do
      p1 <- VSM.unsafeRead plane (baseIdx - 2)
      p0 <- VSM.unsafeRead plane (baseIdx - 1)
      q0 <- VSM.unsafeRead plane baseIdx
      q1 <- VSM.unsafeRead plane (baseIdx + 1)

      when (needsFiltering p1 p0 q0 q1 limit) $ do
        let (!p0', !q0') = simpleFilter p0 q0 p1 q1
        VSM.unsafeWrite plane (baseIdx - 1) p0'
        VSM.unsafeWrite plane baseIdx q0'

-- | Fast horizontal edge simple filter (filters 16 columns at row y).
{-# INLINE filterSimpleHEdgeFast #-}
filterSimpleHEdgeFast :: VSM.MVector s Word8 -> Int -> Int -> (Int, Int) -> Int -> ST s ()
filterSimpleHEdgeFast plane stride planeLen (x, y) limit = do
  forM_ [0 .. 15] $ \i -> do
    let !px = x + i
        !baseIdx = y * stride + px
        !idx_m2 = (y - 2) * stride + px
        !idx_m1 = (y - 1) * stride + px
        !idx_p1 = (y + 1) * stride + px
    -- Check if all indices are in bounds
    when (idx_m2 >= 0 && idx_p1 < planeLen) $ do
      p1 <- VSM.unsafeRead plane idx_m2
      p0 <- VSM.unsafeRead plane idx_m1
      q0 <- VSM.unsafeRead plane baseIdx
      q1 <- VSM.unsafeRead plane idx_p1

      when (needsFiltering p1 p0 q0 q1 limit) $ do
        let (!p0', !q0') = simpleFilter p0 q0 p1 q1
        VSM.unsafeWrite plane idx_m1 p0'
        VSM.unsafeWrite plane baseIdx q0'

{-# INLINE needsFiltering #-}
needsFiltering :: Word8 -> Word8 -> Word8 -> Word8 -> Int -> Bool
needsFiltering p1 p0 q0 q1 limit =
  let !test1 = abs (fromIntegral q0 - fromIntegral p0 :: Int) * 2 + (abs (fromIntegral p1 - fromIntegral q1 :: Int) `shiftR` 1)
   in test1 <= limit

-- | Common filter adjustment with outer taps (RFC 6386 §15.1):
-- @w = clip(clip(p1 - q1) + 3*(q0 - p0))@, @Filter1 = clip(w+4) >> 3@,
-- @Filter2 = clip(w+3) >> 3@. Used for the simple filter and for the
-- HEV path of both normal filters. Note: (p1 - q1) is clamped BEFORE
-- being combined, and Filter1/Filter2 clamp BEFORE shifting.
{-# INLINE simpleFilter #-}
simpleFilter :: Word8 -> Word8 -> Word8 -> Word8 -> (Word8, Word8)
simpleFilter p0 q0 p1 q1 =
  let !a = clipFilter (fromIntegral p1 - fromIntegral q1 :: Int)
      !w = clipFilter (a + 3 * (fromIntegral q0 - fromIntegral p0))
      !filter1 = clipFilter (w + 4) `shiftR` 3
      !filter2 = clipFilter (w + 3) `shiftR` 3
      !p0' = clip255 (fromIntegral p0 + filter2)
      !q0' = clip255 (fromIntegral q0 - filter1)
   in (p0', q0')

{-# INLINE clipFilter #-}
clipFilter :: Int -> Int
clipFilter x = max (-128) (min 127 x)

-- ==========================================================================
-- Normal filter primitives
-- ==========================================================================

-- | Normal filter check with separate edge and interior limits (RFC 6386 §15.2).
{-# INLINE normalFilterCheck #-}
normalFilterCheck :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Int -> Int -> Bool
normalFilterCheck p3 p2 p1 p0 q0 q1 q2 q3 edgeLimit interiorLimit =
  let !edgeTest = abs (fromIntegral p0 - fromIntegral q0 :: Int) * 2 + (abs (fromIntegral p1 - fromIntegral q1 :: Int) `shiftR` 1)
   in edgeTest <= edgeLimit
        && abs (fromIntegral p3 - fromIntegral p2 :: Int) <= interiorLimit
        && abs (fromIntegral p2 - fromIntegral p1 :: Int) <= interiorLimit
        && abs (fromIntegral p1 - fromIntegral p0 :: Int) <= interiorLimit
        && abs (fromIntegral q3 - fromIntegral q2 :: Int) <= interiorLimit
        && abs (fromIntegral q2 - fromIntegral q1 :: Int) <= interiorLimit
        && abs (fromIntegral q1 - fromIntegral q0 :: Int) <= interiorLimit

{-# INLINE isHighEdgeVariance #-}
isHighEdgeVariance :: Word8 -> Word8 -> Word8 -> Word8 -> Int -> Bool
isHighEdgeVariance p1 p0 q0 q1 thresh =
  abs (fromIntegral p1 - fromIntegral p0 :: Int) > thresh || abs (fromIntegral q1 - fromIntegral q0 :: Int) > thresh

-- | Normal subblock filter, non-HEV path (RFC 6386 §15.3):
-- no outer taps (@w = clip(3*(q0 - p0))@); modifies p1/p0/q0/q1.
-- P1/Q1 are adjusted by @(Filter1 + 1) >> 1@.
{-# INLINE subblockFilter #-}
subblockFilter :: Word8 -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8)
subblockFilter p1 p0 q0 q1 =
  let !w = clipFilter (3 * (fromIntegral q0 - fromIntegral p0 :: Int))
      !filter1 = clipFilter (w + 4) `shiftR` 3
      !filter2 = clipFilter (w + 3) `shiftR` 3
      !p0' = clip255 (fromIntegral p0 + filter2)
      !q0' = clip255 (fromIntegral q0 - filter1)

      !a1 = (filter1 + 1) `shiftR` 1
      !p1' = clip255 (fromIntegral p1 + a1)
      !q1' = clip255 (fromIntegral q1 - a1)
   in (p1', p0', q0', q1')

-- | Normal MB-edge filter, non-HEV path (RFC 6386 §15.3):
-- @w = clip(clip(p1 - q1) + 3*(q0 - p0))@ (WITH the clamped outer tap),
-- then 27/18/9-weighted adjustments to p2..q2.
{-# INLINE mbFilter #-}
mbFilter :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8, Word8, Word8)
mbFilter p2 p1 p0 q0 q1 q2 =
  let !outer = clipFilter (fromIntegral p1 - fromIntegral q1 :: Int)
      !w = clipFilter (outer + 3 * (fromIntegral q0 - fromIntegral p0))
      !a1 = (27 * w + 63) `shiftR` 7
      !a2 = (18 * w + 63) `shiftR` 7
      !a3 = (9 * w + 63) `shiftR` 7

      !p2' = clip255 (fromIntegral p2 + a3)
      !p1' = clip255 (fromIntegral p1 + a2)
      !p0' = clip255 (fromIntegral p0 + a1)
      !q0' = clip255 (fromIntegral q0 - a1)
      !q1' = clip255 (fromIntegral q1 - a2)
      !q2' = clip255 (fromIntegral q2 - a3)
   in (p2', p1', p0', q0', q1', q2')

{-# INLINE clip255 #-}
clip255 :: Int -> Word8
clip255 x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = fromIntegral x

-- | Filter vertical MB edge for `span` rows starting at (x, y).
{-# INLINE normalVMBFast #-}
normalVMBFast :: VSM.MVector s Word8 -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s ()
normalVMBFast plane stride planeLen x y span eLimit iLimit hevThresh =
  forM_ [0 .. span - 1] $ \i -> do
    let !base = (y + i) * stride + x
    when (base - 4 >= 0 && base + 3 < planeLen) $ do
      p3 <- VSM.unsafeRead plane (base - 4)
      p2 <- VSM.unsafeRead plane (base - 3)
      p1 <- VSM.unsafeRead plane (base - 2)
      p0 <- VSM.unsafeRead plane (base - 1)
      q0 <- VSM.unsafeRead plane base
      q1 <- VSM.unsafeRead plane (base + 1)
      q2 <- VSM.unsafeRead plane (base + 2)
      q3 <- VSM.unsafeRead plane (base + 3)
      when (normalFilterCheck p3 p2 p1 p0 q0 q1 q2 q3 eLimit iLimit) $ do
        if isHighEdgeVariance p1 p0 q0 q1 hevThresh
          then do
            let (!p0', !q0') = simpleFilter p0 q0 p1 q1
            VSM.unsafeWrite plane (base - 1) p0'
            VSM.unsafeWrite plane base q0'
          else do
            let (!p2', !p1', !p0', !q0', !q1', !q2') = mbFilter p2 p1 p0 q0 q1 q2
            VSM.unsafeWrite plane (base - 3) p2'
            VSM.unsafeWrite plane (base - 2) p1'
            VSM.unsafeWrite plane (base - 1) p0'
            VSM.unsafeWrite plane base q0'
            VSM.unsafeWrite plane (base + 1) q1'
            VSM.unsafeWrite plane (base + 2) q2'

-- | Filter vertical sub-block edge for `span` rows starting at (x, y).
{-# INLINE normalVSubFast #-}
normalVSubFast :: VSM.MVector s Word8 -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s ()
normalVSubFast plane stride planeLen x y span eLimit iLimit hevThresh =
  forM_ [0 .. span - 1] $ \i -> do
    let !base = (y + i) * stride + x
    when (base - 4 >= 0 && base + 3 < planeLen) $ do
      p3 <- VSM.unsafeRead plane (base - 4)
      p2 <- VSM.unsafeRead plane (base - 3)
      p1 <- VSM.unsafeRead plane (base - 2)
      p0 <- VSM.unsafeRead plane (base - 1)
      q0 <- VSM.unsafeRead plane base
      q1 <- VSM.unsafeRead plane (base + 1)
      q2 <- VSM.unsafeRead plane (base + 2)
      q3 <- VSM.unsafeRead plane (base + 3)
      when (normalFilterCheck p3 p2 p1 p0 q0 q1 q2 q3 eLimit iLimit) $ do
        if isHighEdgeVariance p1 p0 q0 q1 hevThresh
          then do
            let (!p0', !q0') = simpleFilter p0 q0 p1 q1
            VSM.unsafeWrite plane (base - 1) p0'
            VSM.unsafeWrite plane base q0'
          else do
            let (!p1', !p0', !q0', !q1') = subblockFilter p1 p0 q0 q1
            VSM.unsafeWrite plane (base - 2) p1'
            VSM.unsafeWrite plane (base - 1) p0'
            VSM.unsafeWrite plane base q0'
            VSM.unsafeWrite plane (base + 1) q1'

-- | Filter horizontal MB edge for `span` columns starting at (x, y).
{-# INLINE normalHMBFast #-}
normalHMBFast :: VSM.MVector s Word8 -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s ()
normalHMBFast plane stride planeLen x y span eLimit iLimit hevThresh =
  forM_ [0 .. span - 1] $ \i -> do
    let !col = x + i
        !base = y * stride + col
        !im4 = base - 4 * stride
        !im3 = base - 3 * stride
        !im2 = base - 2 * stride
        !im1 = base - stride
        !ip1 = base + stride
        !ip2 = base + 2 * stride
        !ip3 = base + 3 * stride
    when (im4 >= 0 && ip3 < planeLen) $ do
      p3 <- VSM.unsafeRead plane im4
      p2 <- VSM.unsafeRead plane im3
      p1 <- VSM.unsafeRead plane im2
      p0 <- VSM.unsafeRead plane im1
      q0 <- VSM.unsafeRead plane base
      q1 <- VSM.unsafeRead plane ip1
      q2 <- VSM.unsafeRead plane ip2
      q3 <- VSM.unsafeRead plane ip3
      when (normalFilterCheck p3 p2 p1 p0 q0 q1 q2 q3 eLimit iLimit) $ do
        if isHighEdgeVariance p1 p0 q0 q1 hevThresh
          then do
            let (!p0', !q0') = simpleFilter p0 q0 p1 q1
            VSM.unsafeWrite plane im1 p0'
            VSM.unsafeWrite plane base q0'
          else do
            let (!p2', !p1', !p0', !q0', !q1', !q2') = mbFilter p2 p1 p0 q0 q1 q2
            VSM.unsafeWrite plane im3 p2'
            VSM.unsafeWrite plane im2 p1'
            VSM.unsafeWrite plane im1 p0'
            VSM.unsafeWrite plane base q0'
            VSM.unsafeWrite plane ip1 q1'
            VSM.unsafeWrite plane ip2 q2'

-- | Filter horizontal sub-block edge for `span` columns starting at (x, y).
{-# INLINE normalHSubFast #-}
normalHSubFast :: VSM.MVector s Word8 -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s ()
normalHSubFast plane stride planeLen x y span eLimit iLimit hevThresh =
  forM_ [0 .. span - 1] $ \i -> do
    let !col = x + i
        !base = y * stride + col
        !im4 = base - 4 * stride
        !im3 = base - 3 * stride
        !im2 = base - 2 * stride
        !im1 = base - stride
        !ip1 = base + stride
        !ip2 = base + 2 * stride
        !ip3 = base + 3 * stride
    when (im4 >= 0 && ip3 < planeLen) $ do
      p3 <- VSM.unsafeRead plane im4
      p2 <- VSM.unsafeRead plane im3
      p1 <- VSM.unsafeRead plane im2
      p0 <- VSM.unsafeRead plane im1
      q0 <- VSM.unsafeRead plane base
      q1 <- VSM.unsafeRead plane ip1
      q2 <- VSM.unsafeRead plane ip2
      q3 <- VSM.unsafeRead plane ip3
      when (normalFilterCheck p3 p2 p1 p0 q0 q1 q2 q3 eLimit iLimit) $ do
        if isHighEdgeVariance p1 p0 q0 q1 hevThresh
          then do
            let (!p0', !q0') = simpleFilter p0 q0 p1 q1
            VSM.unsafeWrite plane im1 p0'
            VSM.unsafeWrite plane base q0'
          else do
            let (!p1', !p0', !q0', !q1') = subblockFilter p1 p0 q0 q1
            VSM.unsafeWrite plane im2 p1'
            VSM.unsafeWrite plane im1 p0'
            VSM.unsafeWrite plane base q0'
            VSM.unsafeWrite plane ip1 q1'

-- ==========================================================================
-- Per-row normal loop filter (encoder reconstruction / strength search)
-- ==========================================================================

-- | Apply normal loop filter to a single MB row for Y, U, and V planes.
-- Uses spec-correct separate edge and interior limits (sharpness = 0,
-- which is what the encoder signals in its frame header). All interior
-- edges are filtered (no per-MB skip information).
applyNormalLoopFilterRow ::
  VSM.MVector s Word8 ->
  Int ->
  VSM.MVector s Word8 ->
  Int ->
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  Int ->
  ST s ()
applyNormalLoopFilterRow yPlane yStride uPlane uStride vPlane vStride mbRow mbCols filterLevel = do
  let !iLimit = max 1 filterLevel -- interior limit (sharpness = 0)
      !mbELimit = (filterLevel + 2) * 2 + iLimit -- MB edge limit
      !subELimit = filterLevel * 2 + iLimit -- sub-block edge limit
      !hevT = hevThresholdFor filterLevel
      !yLen = VSM.length yPlane
      !uLen = VSM.length uPlane
      !vLen = VSM.length vPlane
  normalFilterPlaneRow yPlane yStride yLen mbRow mbCols 16 mbELimit subELimit iLimit hevT
  normalFilterPlaneRow uPlane uStride uLen mbRow mbCols 8 mbELimit subELimit iLimit hevT
  normalFilterPlaneRow vPlane vStride vLen mbRow mbCols 8 mbELimit subELimit iLimit hevT

-- | Filter one plane for one MB row with normal filter.
normalFilterPlaneRow ::
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  ST s ()
normalFilterPlaneRow plane stride planeLen mbRow mbCols blockSize mbELimit subELimit iLimit hevT =
  forM_ [0 .. mbCols - 1] $ \mbX ->
    filterNormalMBPlane plane stride planeLen mbX mbRow blockSize mbELimit subELimit iLimit hevT True

-- ==========================================================================
-- Per-segment loop filter (for encoder with adaptive filter levels)
-- ==========================================================================

-- | Apply normal loop filter to a single MB row with per-segment filter levels.
-- Each MB uses its segment's effective filter level: baseLevel + segFilterDelta[segId].
applyNormalLoopFilterRowSegmented ::
  VSM.MVector s Word8 ->
  Int ->
  VSM.MVector s Word8 ->
  Int ->
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  Int ->
  VU.Vector Int ->
  VU.Vector Word8 ->
  ST s ()
applyNormalLoopFilterRowSegmented yPlane yStride uPlane uStride vPlane vStride mbRow mbCols baseLevel segFilterDeltas segMap = do
  let !yLen = VSM.length yPlane
      !uLen = VSM.length uPlane
      !vLen = VSM.length vPlane
  normalFilterPlaneRowSeg yPlane yStride yLen mbRow mbCols 16 baseLevel segFilterDeltas segMap
  normalFilterPlaneRowSeg uPlane uStride uLen mbRow mbCols 8 baseLevel segFilterDeltas segMap
  normalFilterPlaneRowSeg vPlane vStride vLen mbRow mbCols 8 baseLevel segFilterDeltas segMap

-- | Filter one plane for one MB row with per-segment filter levels.
normalFilterPlaneRowSeg ::
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  VU.Vector Int ->
  VU.Vector Word8 ->
  ST s ()
normalFilterPlaneRowSeg plane stride planeLen mbRow mbCols blockSize baseLevel segFilterDeltas segMap =
  forM_ [0 .. mbCols - 1] $ \mbX -> do
    let !segId = fromIntegral (segMap VU.! (mbRow * mbCols + mbX))
        !level = max 0 $ min 63 $ baseLevel + (segFilterDeltas VU.! segId)
    when (level > 0) $ do
      let !iLimit = max 1 level
          !mbELimit = (level + 2) * 2 + iLimit
          !subELimit = level * 2 + iLimit
          !hevT = hevThresholdFor level
      filterNormalMBPlane plane stride planeLen mbX mbRow blockSize mbELimit subELimit iLimit hevT True
