{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.LoopFilter
  ( applyLoopFilter,
    applySimpleLoopFilterRow,
    applyNormalLoopFilterRow,
  )
where

import Codec.Picture.WebP.Internal.VP8.Header
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import Data.Int
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word

-- | Apply loop filter to reconstructed frame
applyLoopFilter :: VP8FrameHeader -> VSM.MVector s Word8 -> Int -> Int -> ST s ()
applyLoopFilter header yPlane width height = do
  let !filterLevel = vp8FilterLevel header
      !filterType = vp8FilterType header

  when (filterLevel > 0) $ do
    if filterType == 1
      then applySimpleLoopFilter yPlane width height filterLevel
      else applyNormalLoopFilter header yPlane width height

-- | Apply simple loop filter (Y plane only)
applySimpleLoopFilter :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
applySimpleLoopFilter yPlane width height filterLevel = do
  let !limit = filterLevel * 2 + filterLevel
      !mbLimit = limit + 4
      !hevThresh = if filterLevel >= 40 then 2 else if filterLevel >= 15 then 1 else 0
      !planeLen = VSM.length yPlane

  -- All edges in simple filter are MB edges, use mbLimit (limit + 4)
  forM_ [0, 16 .. height - 1] $ \y ->
    forM_ [16, 32 .. width - 1] $ \x ->
      filterSimpleVEdgeFast yPlane width planeLen (x, y) mbLimit hevThresh

  forM_ [16, 32 .. height - 1] $ \y ->
    forM_ [0, 16 .. width - 1] $ \x ->
      filterSimpleHEdgeFast yPlane width planeLen (x, y) mbLimit hevThresh

-- | Apply simple loop filter to a single MB row (for per-row filtering)
-- Matches libwebp's DoFilter per-MB ordering: vertical MB edge, then horizontal MB edge
applySimpleLoopFilterRow :: VSM.MVector s Word8 -> Int -> Int -> Int -> Int -> ST s ()
applySimpleLoopFilterRow yPlane stride mbRow mbCols filterLevel = do
  let !limit = filterLevel * 2 + filterLevel
      !mbLimit = limit + 4
      !planeLen = VSM.length yPlane
  forM_ [0 .. mbCols - 1] $ \mbX -> do
    -- Filter left vertical MB edge (if not leftmost)
    when (mbX > 0) $
      filterSimpleVEdgeFast yPlane stride planeLen (mbX * 16, mbRow * 16) mbLimit 0
    -- Filter top horizontal MB edge (if not topmost)
    when (mbRow > 0) $
      filterSimpleHEdgeFast yPlane stride planeLen (mbX * 16, mbRow * 16) mbLimit 0

-- | Apply normal loop filter (Y, U, V planes)
applyNormalLoopFilter :: VP8FrameHeader -> VSM.MVector s Word8 -> Int -> Int -> ST s ()
applyNormalLoopFilter header yPlane width height = do
  let filterLevel = vp8FilterLevel header
      sharpness = vp8Sharpness header

  let interiorLimit =
        if sharpness > 0
          then
            if sharpness > 4
              then 0
              else 9 - sharpness
          else filterLevel * 2 + filterLevel

      mbEdgeLimit = (filterLevel + 2) * 2 + interiorLimit
      subEdgeLimit = filterLevel * 2 + interiorLimit
      hevThresh = if filterLevel >= 40 then 2 else if filterLevel >= 15 then 1 else 0

  forM_ [0, 16 .. height - 1] $ \y ->
    forM_ [16, 32 .. width - 1] $ \x ->
      filterNormalVEdgeMB yPlane width (x, y) mbEdgeLimit hevThresh

  forM_ [0, 16 .. height - 1] $ \y ->
    forM_ [4, 8 .. width - 1] $ \x ->
      when ((x .&. 15) /= 0) $ -- x `mod` 16 /= 0
        filterNormalVEdgeSub yPlane width (x, y) subEdgeLimit hevThresh

  forM_ [16, 32 .. height - 1] $ \y ->
    forM_ [0, 16 .. width - 1] $ \x ->
      filterNormalHEdgeMB yPlane width (x, y) mbEdgeLimit hevThresh

  forM_ [4, 8 .. height - 1] $ \y ->
    forM_ [0, 16 .. width - 1] $ \x ->
      when ((y .&. 15) /= 0) $ -- y `mod` 16 /= 0
        filterNormalHEdgeSub yPlane width (x, y) subEdgeLimit hevThresh

-- Simple filter functions

-- | Fast vertical edge filter for interior pixels (skips bounds checks)
{-# INLINE filterSimpleVEdgeFast #-}
filterSimpleVEdgeFast :: VSM.MVector s Word8 -> Int -> Int -> (Int, Int) -> Int -> Int -> ST s ()
filterSimpleVEdgeFast plane stride planeLen (x, y) limit _hevThresh = do
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

-- | Fast horizontal edge filter for interior pixels (skips bounds checks)
{-# INLINE filterSimpleHEdgeFast #-}
filterSimpleHEdgeFast :: VSM.MVector s Word8 -> Int -> Int -> (Int, Int) -> Int -> Int -> ST s ()
filterSimpleHEdgeFast plane stride planeLen (x, y) limit _hevThresh = do
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

filterSimpleVEdge :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Int -> Int -> ST s ()
filterSimpleVEdge plane stride (x, y) limit _hevThresh = do
  forM_ [0 .. 15] $ \i -> do
    let py = y + i
    p1 <- readPlane plane stride (x - 2, py)
    p0 <- readPlane plane stride (x - 1, py)
    q0 <- readPlane plane stride (x + 0, py)
    q1 <- readPlane plane stride (x + 1, py)

    when (needsFiltering p1 p0 q0 q1 limit) $ do
      let (p0', q0') = simpleFilter p0 q0 p1 q1
      writePlane plane stride (x - 1, py) p0'
      writePlane plane stride (x + 0, py) q0'

filterSimpleHEdge :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Int -> Int -> ST s ()
filterSimpleHEdge plane stride (x, y) limit _hevThresh = do
  forM_ [0 .. 15] $ \i -> do
    let px = x + i
    p1 <- readPlane plane stride (px, y - 2)
    p0 <- readPlane plane stride (px, y - 1)
    q0 <- readPlane plane stride (px, y + 0)
    q1 <- readPlane plane stride (px, y + 1)

    when (needsFiltering p1 p0 q0 q1 limit) $ do
      let (p0', q0') = simpleFilter p0 q0 p1 q1
      writePlane plane stride (px, y - 1) p0'
      writePlane plane stride (px, y + 0) q0'

{-# INLINE needsFiltering #-}
needsFiltering :: Word8 -> Word8 -> Word8 -> Word8 -> Int -> Bool
needsFiltering p1 p0 q0 q1 limit =
  let !test1 = abs (fromIntegral q0 - fromIntegral p0 :: Int) * 2 + (abs (fromIntegral p1 - fromIntegral q1 :: Int) `shiftR` 1)
   in test1 <= limit

{-# INLINE simpleFilter #-}
simpleFilter :: Word8 -> Word8 -> Word8 -> Word8 -> (Word8, Word8)
simpleFilter p0 q0 p1 q1 =
  let !a = (fromIntegral p1 - fromIntegral q1) :: Int
      !w = clipFilter (3 * (fromIntegral q0 - fromIntegral p0) + a)
      !filter1 = clipFilter ((w + 4) `shiftR` 3)
      !filter2 = clipFilter ((w + 3) `shiftR` 3)
      !p0' = clip255 (fromIntegral p0 + filter2)
      !q0' = clip255 (fromIntegral q0 - filter1)
   in (p0', q0')

{-# INLINE clipFilter #-}
clipFilter :: Int -> Int
clipFilter x = max (-128) (min 127 x)

-- Normal filter functions

filterNormalVEdgeMB :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Int -> Int -> ST s ()
filterNormalVEdgeMB plane stride (x, y) limit hevThresh = do
  forM_ [0 .. 15] $ \i -> do
    let py = y + i
    p3 <- readPlane plane stride (x - 4, py)
    p2 <- readPlane plane stride (x - 3, py)
    p1 <- readPlane plane stride (x - 2, py)
    p0 <- readPlane plane stride (x - 1, py)
    q0 <- readPlane plane stride (x + 0, py)
    q1 <- readPlane plane stride (x + 1, py)
    q2 <- readPlane plane stride (x + 2, py)
    q3 <- readPlane plane stride (x + 3, py)

    when (needsFilteringNormal p3 p2 p1 p0 q0 q1 q2 q3 limit) $ do
      let hev = isHighEdgeVariance p1 p0 q0 q1 hevThresh

      if hev
        then do
          let (p0', q0') = simpleFilter p0 q0 p1 q1
          writePlane plane stride (x - 1, py) p0'
          writePlane plane stride (x + 0, py) q0'
        else do
          let (p2', p1', p0', q0', q1', q2') = mbFilter p2 p1 p0 q0 q1 q2
          writePlane plane stride (x - 3, py) p2'
          writePlane plane stride (x - 2, py) p1'
          writePlane plane stride (x - 1, py) p0'
          writePlane plane stride (x + 0, py) q0'
          writePlane plane stride (x + 1, py) q1'
          writePlane plane stride (x + 2, py) q2'

filterNormalHEdgeMB :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Int -> Int -> ST s ()
filterNormalHEdgeMB plane stride (x, y) limit hevThresh = do
  forM_ [0 .. 15] $ \i -> do
    let px = x + i
    p3 <- readPlane plane stride (px, y - 4)
    p2 <- readPlane plane stride (px, y - 3)
    p1 <- readPlane plane stride (px, y - 2)
    p0 <- readPlane plane stride (px, y - 1)
    q0 <- readPlane plane stride (px, y + 0)
    q1 <- readPlane plane stride (px, y + 1)
    q2 <- readPlane plane stride (px, y + 2)
    q3 <- readPlane plane stride (px, y + 3)

    when (needsFilteringNormal p3 p2 p1 p0 q0 q1 q2 q3 limit) $ do
      let hev = isHighEdgeVariance p1 p0 q0 q1 hevThresh

      if hev
        then do
          let (p0', q0') = simpleFilter p0 q0 p1 q1
          writePlane plane stride (px, y - 1) p0'
          writePlane plane stride (px, y + 0) q0'
        else do
          let (p2', p1', p0', q0', q1', q2') = mbFilter p2 p1 p0 q0 q1 q2
          writePlane plane stride (px, y - 3) p2'
          writePlane plane stride (px, y - 2) p1'
          writePlane plane stride (px, y - 1) p0'
          writePlane plane stride (px, y + 0) q0'
          writePlane plane stride (px, y + 1) q1'
          writePlane plane stride (px, y + 2) q2'

filterNormalVEdgeSub :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Int -> Int -> ST s ()
filterNormalVEdgeSub plane stride (x, y) limit hevThresh = do
  forM_ [0 .. 15] $ \i -> do
    let py = y + i
    p2 <- readPlane plane stride (x - 3, py)
    p1 <- readPlane plane stride (x - 2, py)
    p0 <- readPlane plane stride (x - 1, py)
    q0 <- readPlane plane stride (x + 0, py)
    q1 <- readPlane plane stride (x + 1, py)
    q2 <- readPlane plane stride (x + 2, py)

    when (needsFilteringNormal p2 p2 p1 p0 q0 q1 q2 q2 limit) $ do
      let hev = isHighEdgeVariance p1 p0 q0 q1 hevThresh

      if hev
        then do
          let (p0', q0') = simpleFilter p0 q0 p1 q1
          writePlane plane stride (x - 1, py) p0'
          writePlane plane stride (x + 0, py) q0'
        else do
          let (p1', p0', q0', q1') = subblockFilter p1 p0 q0 q1
          writePlane plane stride (x - 2, py) p1'
          writePlane plane stride (x - 1, py) p0'
          writePlane plane stride (x + 0, py) q0'
          writePlane plane stride (x + 1, py) q1'

filterNormalHEdgeSub :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Int -> Int -> ST s ()
filterNormalHEdgeSub plane stride (x, y) limit hevThresh = do
  forM_ [0 .. 15] $ \i -> do
    let px = x + i
    p2 <- readPlane plane stride (px, y - 3)
    p1 <- readPlane plane stride (px, y - 2)
    p0 <- readPlane plane stride (px, y - 1)
    q0 <- readPlane plane stride (px, y + 0)
    q1 <- readPlane plane stride (px, y + 1)
    q2 <- readPlane plane stride (px, y + 2)

    when (needsFilteringNormal p2 p2 p1 p0 q0 q1 q2 q2 limit) $ do
      let hev = isHighEdgeVariance p1 p0 q0 q1 hevThresh

      if hev
        then do
          let (p0', q0') = simpleFilter p0 q0 p1 q1
          writePlane plane stride (px, y - 1) p0'
          writePlane plane stride (px, y + 0) q0'
        else do
          let (p1', p0', q0', q1') = subblockFilter p1 p0 q0 q1
          writePlane plane stride (px, y - 2) p1'
          writePlane plane stride (px, y - 1) p0'
          writePlane plane stride (px, y + 0) q0'
          writePlane plane stride (px, y + 1) q1'

{-# INLINE needsFilteringNormal #-}
needsFilteringNormal :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Int -> Bool
needsFilteringNormal p3 p2 p1 p0 q0 q1 q2 q3 limit =
  let !test1 = abs (fromIntegral p0 - fromIntegral q0 :: Int) * 2 + (abs (fromIntegral p1 - fromIntegral q1 :: Int) `shiftR` 1)
   in test1 <= limit
        && abs (fromIntegral p3 - fromIntegral p2 :: Int) <= limit
        && abs (fromIntegral p2 - fromIntegral p1 :: Int) <= limit
        && abs (fromIntegral p1 - fromIntegral p0 :: Int) <= limit
        && abs (fromIntegral q3 - fromIntegral q2 :: Int) <= limit
        && abs (fromIntegral q2 - fromIntegral q1 :: Int) <= limit
        && abs (fromIntegral q1 - fromIntegral q0 :: Int) <= limit

{-# INLINE isHighEdgeVariance #-}
isHighEdgeVariance :: Word8 -> Word8 -> Word8 -> Word8 -> Int -> Bool
isHighEdgeVariance p1 p0 q0 q1 thresh =
  abs (fromIntegral p1 - fromIntegral p0 :: Int) > thresh || abs (fromIntegral q1 - fromIntegral q0 :: Int) > thresh

{-# INLINE subblockFilter #-}
subblockFilter :: Word8 -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8)
subblockFilter p1 p0 q0 q1 =
  let !a = (fromIntegral p1 - fromIntegral q1) :: Int
      !w = clipFilter (3 * (fromIntegral q0 - fromIntegral p0) + a)
      !filter1 = clipFilter ((w + 4) `shiftR` 3)
      !filter2 = clipFilter ((w + 3) `shiftR` 3)
      !p0' = clip255 (fromIntegral p0 + filter2)
      !q0' = clip255 (fromIntegral q0 - filter1)

      !a1 = (filter1 + 1) `shiftR` 1
      !p1' = clip255 (fromIntegral p1 + a1)
      !q1' = clip255 (fromIntegral q1 - a1)
   in (p1', p0', q0', q1')

{-# INLINE mbFilter #-}
mbFilter :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8, Word8, Word8)
mbFilter p2 p1 p0 q0 q1 q2 =
  let !w = clipFilter (3 * (fromIntegral q0 - fromIntegral p0 :: Int))
      !a = (27 * w + 63) `shiftR` 7

      !p2' = clip255 (fromIntegral p2 + ((9 * w + 63) `shiftR` 7))
      !p1' = clip255 (fromIntegral p1 + ((18 * w + 63) `shiftR` 7))
      !p0' = clip255 (fromIntegral p0 + a)
      !q0' = clip255 (fromIntegral q0 - a)
      !q1' = clip255 (fromIntegral q1 - ((18 * w + 63) `shiftR` 7))
      !q2' = clip255 (fromIntegral q2 - ((9 * w + 63) `shiftR` 7))
   in (p2', p1', p0', q0', q1', q2')

-- Helper functions

{-# INLINE readPlane #-}
readPlane :: VSM.MVector s Word8 -> Int -> (Int, Int) -> ST s Word8
readPlane plane stride (x, y)
  | x < 0 || y < 0 = return 0
  | otherwise = do
      let !idx = y * stride + x
      if idx >= 0 && idx < VSM.length plane
        then VSM.unsafeRead plane idx
        else return 0

{-# INLINE writePlane #-}
writePlane :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Word8 -> ST s ()
writePlane plane stride (x, y) val = do
  let !idx = y * stride + x
  when (idx >= 0 && idx < VSM.length plane) $
    VSM.unsafeWrite plane idx val

{-# INLINE clip255 #-}
clip255 :: Int -> Word8
clip255 x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = fromIntegral x

-- ==========================================================================
-- Per-row normal loop filter (for encoder and decoder)
-- ==========================================================================

-- | Apply normal loop filter to a single MB row for Y, U, and V planes.
-- Uses spec-correct separate edge and interior limits (sharpness = 0).
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
  let !iLimit = filterLevel -- interior limit (sharpness = 0)
      !mbELimit = 3 * filterLevel + 4 -- MB edge limit
      !subELimit = 3 * filterLevel -- sub-block edge limit
      !hevT = if filterLevel >= 40 then 2 else if filterLevel >= 15 then 1 else 0
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
  forM_ [0 .. mbCols - 1] $ \mbX -> do
    let !bx = mbX * blockSize
        !by = mbRow * blockSize
    -- Vertical MB edge
    when (mbX > 0) $
      normalVMBFast plane stride planeLen bx by blockSize mbELimit iLimit hevT
    -- Vertical sub-block edges (every 4 pixels within MB)
    forM_ [4, 8 .. blockSize - 1] $ \dx ->
      normalVSubFast plane stride planeLen (bx + dx) by blockSize subELimit iLimit hevT
    -- Horizontal MB edge
    when (mbRow > 0) $
      normalHMBFast plane stride planeLen bx by blockSize mbELimit iLimit hevT
    -- Horizontal sub-block edges (every 4 pixels within MB)
    forM_ [4, 8 .. blockSize - 1] $ \dy ->
      normalHSubFast plane stride planeLen bx (by + dy) blockSize subELimit iLimit hevT

-- | Normal filter check with separate edge and interior limits (spec-correct).
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
    when (base - 3 >= 0 && base + 2 < planeLen) $ do
      p2 <- VSM.unsafeRead plane (base - 3)
      p1 <- VSM.unsafeRead plane (base - 2)
      p0 <- VSM.unsafeRead plane (base - 1)
      q0 <- VSM.unsafeRead plane base
      q1 <- VSM.unsafeRead plane (base + 1)
      q2 <- VSM.unsafeRead plane (base + 2)
      when (normalFilterCheck p2 p2 p1 p0 q0 q1 q2 q2 eLimit iLimit) $ do
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
        !im3 = base - 3 * stride
        !im2 = base - 2 * stride
        !im1 = base - stride
        !ip1 = base + stride
        !ip2 = base + 2 * stride
    when (im3 >= 0 && ip2 < planeLen) $ do
      p2 <- VSM.unsafeRead plane im3
      p1 <- VSM.unsafeRead plane im2
      p0 <- VSM.unsafeRead plane im1
      q0 <- VSM.unsafeRead plane base
      q1 <- VSM.unsafeRead plane ip1
      q2 <- VSM.unsafeRead plane ip2
      when (normalFilterCheck p2 p2 p1 p0 q0 q1 q2 q2 eLimit iLimit) $ do
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
