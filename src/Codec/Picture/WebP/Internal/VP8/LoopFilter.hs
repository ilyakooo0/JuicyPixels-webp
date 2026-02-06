{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.LoopFilter
  ( applyLoopFilter
  )
where

import Codec.Picture.WebP.Internal.VP8.Header
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word
import Data.Int

-- | Apply loop filter to reconstructed frame
applyLoopFilter :: VP8FrameHeader -> VSM.MVector s Word8 -> Int -> Int -> ST s ()
applyLoopFilter header yPlane width height = do
  let filterLevel = vp8FilterLevel header
      filterType = vp8FilterType header

  when (filterLevel > 0) $ do
    if filterType == 1
      then applySimpleLoopFilter yPlane width height filterLevel
      else applyNormalLoopFilter header yPlane width height

-- | Apply simple loop filter (Y plane only)
applySimpleLoopFilter :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
applySimpleLoopFilter yPlane width height filterLevel = do
  let limit = filterLevel * 2 + filterLevel
      hevThresh = if filterLevel >= 40 then 2 else if filterLevel >= 15 then 1 else 0

  forM_ [0, 16 .. height - 1] $ \y ->
    forM_ [16, 32 .. width - 1] $ \x ->
      filterSimpleVEdge yPlane width (x, y) limit hevThresh

  forM_ [16, 32 .. height - 1] $ \y ->
    forM_ [0, 16 .. width - 1] $ \x ->
      filterSimpleHEdge yPlane width (x, y) limit hevThresh

-- | Apply normal loop filter (Y, U, V planes)
applyNormalLoopFilter :: VP8FrameHeader -> VSM.MVector s Word8 -> Int -> Int -> ST s ()
applyNormalLoopFilter header yPlane width height = do
  let filterLevel = vp8FilterLevel header
      sharpness = vp8Sharpness header

  let interiorLimit = if sharpness > 0
        then if sharpness > 4
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
      when (x `mod` 16 /= 0) $
        filterNormalVEdgeSub yPlane width (x, y) subEdgeLimit hevThresh

  forM_ [16, 32 .. height - 1] $ \y ->
    forM_ [0, 16 .. width - 1] $ \x ->
      filterNormalHEdgeMB yPlane width (x, y) mbEdgeLimit hevThresh

  forM_ [4, 8 .. height - 1] $ \y ->
    forM_ [0, 16 .. width - 1] $ \x ->
      when (y `mod` 16 /= 0) $
        filterNormalHEdgeSub yPlane width (x, y) subEdgeLimit hevThresh

-- Simple filter functions

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

needsFiltering :: Word8 -> Word8 -> Word8 -> Word8 -> Int -> Bool
needsFiltering p1 p0 q0 q1 limit =
  let test1 = abs (fromIntegral q0 - fromIntegral p0 :: Int) * 2 + (abs (fromIntegral p1 - fromIntegral q1 :: Int) `shiftR` 1)
   in test1 <= limit

simpleFilter :: Word8 -> Word8 -> Word8 -> Word8 -> (Word8, Word8)
simpleFilter p0 q0 p1 q1 =
  let a = (fromIntegral p1 - fromIntegral q1) :: Int
      w = clip (3 * (fromIntegral q0 - fromIntegral p0) + a)
      filter1 = clip ((w + 4) `shiftR` 3)
      filter2 = clip ((w + 3) `shiftR` 3)
      p0' = clip255 (fromIntegral p0 + filter2)
      q0' = clip255 (fromIntegral q0 - filter1)
   in (p0', q0')
  where
    clip x = max (-128) (min 127 x)

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

needsFilteringNormal :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Int -> Bool
needsFilteringNormal p3 p2 p1 p0 q0 q1 q2 q3 limit =
  let test1 = abs (fromIntegral p0 - fromIntegral q0 :: Int) * 2 + (abs (fromIntegral p1 - fromIntegral q1 :: Int) `shiftR` 1)
      test2 = abs (fromIntegral p3 - fromIntegral p2 :: Int) <= limit
      test3 = abs (fromIntegral p2 - fromIntegral p1 :: Int) <= limit
      test4 = abs (fromIntegral p1 - fromIntegral p0 :: Int) <= limit
      test5 = abs (fromIntegral q3 - fromIntegral q2 :: Int) <= limit
      test6 = abs (fromIntegral q2 - fromIntegral q1 :: Int) <= limit
      test7 = abs (fromIntegral q1 - fromIntegral q0 :: Int) <= limit
   in test1 <= limit && test2 && test3 && test4 && test5 && test6 && test7

isHighEdgeVariance :: Word8 -> Word8 -> Word8 -> Word8 -> Int -> Bool
isHighEdgeVariance p1 p0 q0 q1 thresh =
  abs (fromIntegral p1 - fromIntegral p0 :: Int) > thresh || abs (fromIntegral q1 - fromIntegral q0 :: Int) > thresh

subblockFilter :: Word8 -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8)
subblockFilter p1 p0 q0 q1 =
  let a = (fromIntegral p1 - fromIntegral q1) :: Int
      w = clip (3 * (fromIntegral q0 - fromIntegral p0) + a)
      filter1 = clip ((w + 4) `shiftR` 3)
      filter2 = clip ((w + 3) `shiftR` 3)
      p0' = clip255 (fromIntegral p0 + filter2)
      q0' = clip255 (fromIntegral q0 - filter1)

      a1 = (filter1 + 1) `shiftR` 1
      p1' = clip255 (fromIntegral p1 + a1)
      q1' = clip255 (fromIntegral q1 - a1)
   in (p1', p0', q0', q1')
  where
    clip x = max (-128) (min 127 x)

mbFilter :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8, Word8, Word8)
mbFilter p2 p1 p0 q0 q1 q2 =
  let w = clip (3 * (fromIntegral q0 - fromIntegral p0 :: Int))
      a = (27 * w + 63) `shiftR` 7

      p2' = clip255 (fromIntegral p2 + ((9 * w + 63) `shiftR` 7))
      p1' = clip255 (fromIntegral p1 + ((18 * w + 63) `shiftR` 7))
      p0' = clip255 (fromIntegral p0 + a)
      q0' = clip255 (fromIntegral q0 - a)
      q1' = clip255 (fromIntegral q1 - ((18 * w + 63) `shiftR` 7))
      q2' = clip255 (fromIntegral q2 - ((9 * w + 63) `shiftR` 7))
   in (p2', p1', p0', q0', q1', q2')
  where
    clip x = max (-128) (min 127 x)

-- Helper functions

readPlane :: VSM.MVector s Word8 -> Int -> (Int, Int) -> ST s Word8
readPlane plane stride (x, y)
  | x < 0 || y < 0 = return 0
  | otherwise = do
      let idx = y * stride + x
      if idx >= 0 && idx < VSM.length plane
        then VSM.read plane idx
        else return 0

writePlane :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Word8 -> ST s ()
writePlane plane stride (x, y) val = do
  let idx = y * stride + x
  when (idx >= 0 && idx < VSM.length plane) $
    VSM.write plane idx val

clip255 :: Int -> Word8
clip255 x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = fromIntegral x
