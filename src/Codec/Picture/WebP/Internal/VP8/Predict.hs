{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.Predict
  ( predict16x16,
    predict8x8,
    predict4x4,
  )
where

import Control.Monad (forM_)
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word

-- | 16x16 luma prediction modes
-- Mode 0: DC_PRED, 1: V_PRED, 2: H_PRED, 3: TM_PRED
predict16x16 :: Int -> VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict16x16 mode output stride x y
  | mode == 0 = predict16x16DC output stride x y
  | mode == 1 = predict16x16V output stride x y
  | mode == 2 = predict16x16H output stride x y
  | mode == 3 = predict16x16TM output stride x y
  | otherwise = return ()

-- | 8x8 chroma prediction modes (same numbering as 16x16)
predict8x8 :: Int -> VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict8x8 mode output stride x y
  | mode == 0 = predict8x8DC output stride x y
  | mode == 1 = predict8x8V output stride x y
  | mode == 2 = predict8x8H output stride x y
  | mode == 3 = predict8x8TM output stride x y
  | otherwise = return ()

-- | 4x4 luma sub-block prediction (10 modes)
predict4x4 :: Int -> VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict4x4 mode output stride x y
  | mode == 0 = predict4x4DC output stride x y
  | mode == 1 = predict4x4TM output stride x y
  | mode == 2 = predict4x4VE output stride x y
  | mode == 3 = predict4x4HE output stride x y
  | mode == 4 = predict4x4LD output stride x y
  | mode == 5 = predict4x4RD output stride x y
  | mode == 6 = predict4x4VR output stride x y
  | mode == 7 = predict4x4VL output stride x y
  | mode == 8 = predict4x4HD output stride x y
  | mode == 9 = predict4x4HU output stride x y
  | otherwise = return ()

-- 16x16 prediction modes

predict16x16DC :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict16x16DC output stride x y = do
  let topAvailable = y > 0
      leftAvailable = x > 0

  sum <-
    if topAvailable && leftAvailable
      then do
        topSum <- sumRow output stride (x, y - 1) 16
        leftSum <- sumCol output stride (x - 1, y) 16
        return $ (topSum + leftSum + 16) `shiftR` 5
      else
        if topAvailable
          then do
            topSum <- sumRow output stride (x, y - 1) 16
            return $ (topSum + 8) `shiftR` 4
          else
            if leftAvailable
              then do
                leftSum <- sumCol output stride (x - 1, y) 16
                return $ (leftSum + 8) `shiftR` 4
              else return 128

  fillBlock output stride (x, y) 16 16 sum

predict16x16V :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict16x16V output stride x y =
  forM_ [0 .. 15] $ \row ->
    forM_ [0 .. 15] $ \col -> do
      above <- readPixel output stride (x + col) (y - 1)
      writePixel output stride (x + col, y + row) above

predict16x16H :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict16x16H output stride x y =
  forM_ [0 .. 15] $ \row ->
    forM_ [0 .. 15] $ \col -> do
      left <- readPixel output stride (x - 1) (y + row)
      writePixel output stride (x + col, y + row) left

predict16x16TM :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict16x16TM output stride x y = do
  topLeft <- readPixel output stride (x - 1) (y - 1)
  forM_ [0 .. 15] $ \row ->
    forM_ [0 .. 15] $ \col -> do
      above <- readPixel output stride (x + col) (y - 1)
      left <- readPixel output stride (x - 1) (y + row)
      let pred = clip255 (fromIntegral above + fromIntegral left - fromIntegral topLeft)
      writePixel output stride (x + col, y + row) pred

-- 8x8 prediction modes

predict8x8DC :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict8x8DC output stride x y = do
  let topAvailable = y > 0
      leftAvailable = x > 0

  sum <-
    if topAvailable && leftAvailable
      then do
        topSum <- sumRow output stride (x, y - 1) 8
        leftSum <- sumCol output stride (x - 1, y) 8
        return $ (topSum + leftSum + 8) `shiftR` 4
      else
        if topAvailable
          then do
            topSum <- sumRow output stride (x, y - 1) 8
            return $ (topSum + 4) `shiftR` 3
          else
            if leftAvailable
              then do
                leftSum <- sumCol output stride (x - 1, y) 8
                return $ (leftSum + 4) `shiftR` 3
              else return 128

  fillBlock output stride (x, y) 8 8 sum

predict8x8V :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict8x8V output stride x y =
  forM_ [0 .. 7] $ \row ->
    forM_ [0 .. 7] $ \col -> do
      above <- readPixel output stride (x + col) (y - 1)
      writePixel output stride (x + col, y + row) above

predict8x8H :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict8x8H output stride x y =
  forM_ [0 .. 7] $ \row ->
    forM_ [0 .. 7] $ \col -> do
      left <- readPixel output stride (x - 1) (y + row)
      writePixel output stride (x + col, y + row) left

predict8x8TM :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict8x8TM output stride x y = do
  topLeft <- readPixel output stride (x - 1) (y - 1)
  forM_ [0 .. 7] $ \row ->
    forM_ [0 .. 7] $ \col -> do
      above <- readPixel output stride (x + col) (y - 1)
      left <- readPixel output stride (x - 1) (y + row)
      let pred = clip255 (fromIntegral above + fromIntegral left - fromIntegral topLeft)
      writePixel output stride (x + col, y + row) pred

-- 4x4 prediction modes
--
-- Edge pixel naming (RFC 6386 Section 12.3):
--
--     M  A  B  C  D  E  F  G  H
--     --------------------------
-- I | p0 p1 p2 p3
-- J | p4 p5 p6 p7
-- K | p8 p9 pA pB
-- L | pC pD pE pF
--
-- A..D come from the row directly above the subblock, E..H are the
-- "above-right" pixels (see readTopRight), I..L are the column to the left
-- and M is the above-left corner pixel.

predict4x4DC :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict4x4DC output stride x y = do
  let topAvailable = y > 0
      leftAvailable = x > 0

  sum <-
    if topAvailable && leftAvailable
      then do
        topSum <- sumRow output stride (x, y - 1) 4
        leftSum <- sumCol output stride (x - 1, y) 4
        return $ (topSum + leftSum + 4) `shiftR` 3
      else
        if topAvailable
          then do
            topSum <- sumRow output stride (x, y - 1) 4
            return $ (topSum + 2) `shiftR` 2
          else
            if leftAvailable
              then do
                leftSum <- sumCol output stride (x - 1, y) 4
                return $ (leftSum + 2) `shiftR` 2
              else return 128

  fillBlock output stride (x, y) 4 4 sum

predict4x4TM :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict4x4TM output stride x y = do
  topLeft <- readPixel output stride (x - 1) (y - 1)
  forM_ [0 .. 3] $ \row ->
    forM_ [0 .. 3] $ \col -> do
      above <- readPixel output stride (x + col) (y - 1)
      left <- readPixel output stride (x - 1) (y + row)
      let pred = clip255 (fromIntegral above + fromIntegral left - fromIntegral topLeft)
      writePixel output stride (x + col, y + row) pred

predict4x4VE :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict4x4VE output stride x y = do
  m <- readPixel output stride (x - 1) (y - 1)
  a <- readPixel output stride (x + 0) (y - 1)
  b <- readPixel output stride (x + 1) (y - 1)
  c <- readPixel output stride (x + 2) (y - 1)
  d <- readPixel output stride (x + 3) (y - 1)
  e <- readTopRight output stride x y 0

  let p0 = avg3 m a b
      p1 = avg3 a b c
      p2 = avg3 b c d
      p3 = avg3 c d e

  forM_ [0 .. 3] $ \row -> do
    writePixel output stride (x + 0, y + row) p0
    writePixel output stride (x + 1, y + row) p1
    writePixel output stride (x + 2, y + row) p2
    writePixel output stride (x + 3, y + row) p3

predict4x4HE :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict4x4HE output stride x y = do
  m <- readPixel output stride (x - 1) (y - 1)
  i <- readPixel output stride (x - 1) (y + 0)
  j <- readPixel output stride (x - 1) (y + 1)
  k <- readPixel output stride (x - 1) (y + 2)
  l <- readPixel output stride (x - 1) (y + 3)

  let p0 = avg3 m i j
      p1 = avg3 i j k
      p2 = avg3 j k l
      p3 = avg3 k l l -- L repeated, no pixel below L
  forM_ [0 .. 3] $ \col -> do
    writePixel output stride (x + col, y + 0) p0
    writePixel output stride (x + col, y + 1) p1
    writePixel output stride (x + col, y + 2) p2
    writePixel output stride (x + col, y + 3) p3

predict4x4LD :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict4x4LD output stride x y = do
  a <- readPixel output stride (x + 0) (y - 1)
  b <- readPixel output stride (x + 1) (y - 1)
  c <- readPixel output stride (x + 2) (y - 1)
  d <- readPixel output stride (x + 3) (y - 1)
  e <- readTopRight output stride x y 0
  f <- readTopRight output stride x y 1
  g <- readTopRight output stride x y 2
  h <- readTopRight output stride x y 3

  writePixel output stride (x + 0, y + 0) (avg3 a b c)
  writePixel output stride (x + 1, y + 0) (avg3 b c d)
  writePixel output stride (x + 2, y + 0) (avg3 c d e)
  writePixel output stride (x + 3, y + 0) (avg3 d e f)

  writePixel output stride (x + 0, y + 1) (avg3 b c d)
  writePixel output stride (x + 1, y + 1) (avg3 c d e)
  writePixel output stride (x + 2, y + 1) (avg3 d e f)
  writePixel output stride (x + 3, y + 1) (avg3 e f g)

  writePixel output stride (x + 0, y + 2) (avg3 c d e)
  writePixel output stride (x + 1, y + 2) (avg3 d e f)
  writePixel output stride (x + 2, y + 2) (avg3 e f g)
  writePixel output stride (x + 3, y + 2) (avg3 f g h)

  writePixel output stride (x + 0, y + 3) (avg3 d e f)
  writePixel output stride (x + 1, y + 3) (avg3 e f g)
  writePixel output stride (x + 2, y + 3) (avg3 f g h)
  writePixel output stride (x + 3, y + 3) (avg3 g h h)

predict4x4RD :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict4x4RD output stride x y = do
  m <- readPixel output stride (x - 1) (y - 1)
  a <- readPixel output stride (x + 0) (y - 1)
  b <- readPixel output stride (x + 1) (y - 1)
  c <- readPixel output stride (x + 2) (y - 1)
  d <- readPixel output stride (x + 3) (y - 1)
  i <- readPixel output stride (x - 1) (y + 0)
  j <- readPixel output stride (x - 1) (y + 1)
  k <- readPixel output stride (x - 1) (y + 2)
  l <- readPixel output stride (x - 1) (y + 3)

  writePixel output stride (x + 0, y + 3) (avg3 j k l)
  writePixel output stride (x + 0, y + 2) (avg3 i j k)
  writePixel output stride (x + 0, y + 1) (avg3 m i j)
  writePixel output stride (x + 0, y + 0) (avg3 a m i)

  writePixel output stride (x + 1, y + 3) (avg3 i j k)
  writePixel output stride (x + 1, y + 2) (avg3 m i j)
  writePixel output stride (x + 1, y + 1) (avg3 a m i)
  writePixel output stride (x + 1, y + 0) (avg3 b a m)

  writePixel output stride (x + 2, y + 3) (avg3 m i j)
  writePixel output stride (x + 2, y + 2) (avg3 a m i)
  writePixel output stride (x + 2, y + 1) (avg3 b a m)
  writePixel output stride (x + 2, y + 0) (avg3 c b a)

  writePixel output stride (x + 3, y + 3) (avg3 a m i)
  writePixel output stride (x + 3, y + 2) (avg3 b a m)
  writePixel output stride (x + 3, y + 1) (avg3 c b a)
  writePixel output stride (x + 3, y + 0) (avg3 d c b)

predict4x4VR :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict4x4VR output stride x y = do
  m <- readPixel output stride (x - 1) (y - 1)
  a <- readPixel output stride (x + 0) (y - 1)
  b <- readPixel output stride (x + 1) (y - 1)
  c <- readPixel output stride (x + 2) (y - 1)
  d <- readPixel output stride (x + 3) (y - 1)
  i <- readPixel output stride (x - 1) (y + 0)
  j <- readPixel output stride (x - 1) (y + 1)
  k <- readPixel output stride (x - 1) (y + 2)

  writePixel output stride (x + 0, y + 0) (avg2 m a)
  writePixel output stride (x + 1, y + 0) (avg2 a b)
  writePixel output stride (x + 2, y + 0) (avg2 b c)
  writePixel output stride (x + 3, y + 0) (avg2 c d)

  writePixel output stride (x + 0, y + 1) (avg3 i m a)
  writePixel output stride (x + 1, y + 1) (avg3 m a b)
  writePixel output stride (x + 2, y + 1) (avg3 a b c)
  writePixel output stride (x + 3, y + 1) (avg3 b c d)

  -- Rows 2-3 shift row 0/row 1 right by one; column 0 comes from the left edge
  writePixel output stride (x + 0, y + 2) (avg3 j i m)
  writePixel output stride (x + 1, y + 2) (avg2 m a) -- = pred[0][0]
  writePixel output stride (x + 2, y + 2) (avg2 a b) -- = pred[0][1]
  writePixel output stride (x + 3, y + 2) (avg2 b c) -- = pred[0][2]
  writePixel output stride (x + 0, y + 3) (avg3 k j i)
  writePixel output stride (x + 1, y + 3) (avg3 i m a) -- = pred[1][0]
  writePixel output stride (x + 2, y + 3) (avg3 m a b) -- = pred[1][1]
  writePixel output stride (x + 3, y + 3) (avg3 a b c) -- = pred[1][2]

predict4x4VL :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict4x4VL output stride x y = do
  a <- readPixel output stride (x + 0) (y - 1)
  b <- readPixel output stride (x + 1) (y - 1)
  c <- readPixel output stride (x + 2) (y - 1)
  d <- readPixel output stride (x + 3) (y - 1)
  e <- readTopRight output stride x y 0
  f <- readTopRight output stride x y 1
  g <- readTopRight output stride x y 2
  h <- readTopRight output stride x y 3

  writePixel output stride (x + 0, y + 0) (avg2 a b)
  writePixel output stride (x + 1, y + 0) (avg2 b c)
  writePixel output stride (x + 2, y + 0) (avg2 c d)
  writePixel output stride (x + 3, y + 0) (avg2 d e)

  writePixel output stride (x + 0, y + 1) (avg3 a b c)
  writePixel output stride (x + 1, y + 1) (avg3 b c d)
  writePixel output stride (x + 2, y + 1) (avg3 c d e)
  writePixel output stride (x + 3, y + 1) (avg3 d e f)

  writePixel output stride (x + 0, y + 2) (avg2 b c)
  writePixel output stride (x + 1, y + 2) (avg2 c d)
  writePixel output stride (x + 2, y + 2) (avg2 d e)
  writePixel output stride (x + 3, y + 2) (avg3 e f g) -- breaks pattern (not avg2(E,F))
  writePixel output stride (x + 0, y + 3) (avg3 b c d)
  writePixel output stride (x + 1, y + 3) (avg3 c d e)
  writePixel output stride (x + 2, y + 3) (avg3 d e f)
  writePixel output stride (x + 3, y + 3) (avg3 f g h) -- breaks pattern (not avg3(E,F,G))

predict4x4HD :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict4x4HD output stride x y = do
  m <- readPixel output stride (x - 1) (y - 1)
  a <- readPixel output stride (x + 0) (y - 1)
  b <- readPixel output stride (x + 1) (y - 1)
  c <- readPixel output stride (x + 2) (y - 1)
  i <- readPixel output stride (x - 1) (y + 0)
  j <- readPixel output stride (x - 1) (y + 1)
  k <- readPixel output stride (x - 1) (y + 2)
  l <- readPixel output stride (x - 1) (y + 3)

  writePixel output stride (x + 0, y + 0) (avg2 m i)
  writePixel output stride (x + 1, y + 0) (avg3 a m i)
  writePixel output stride (x + 2, y + 0) (avg3 b a m)
  writePixel output stride (x + 3, y + 0) (avg3 c b a)

  -- Each row shifts the row above right by two
  writePixel output stride (x + 0, y + 1) (avg2 i j)
  writePixel output stride (x + 1, y + 1) (avg3 m i j)
  writePixel output stride (x + 2, y + 1) (avg2 m i) -- = pred[0][0]
  writePixel output stride (x + 3, y + 1) (avg3 a m i) -- = pred[0][1]
  writePixel output stride (x + 0, y + 2) (avg2 j k)
  writePixel output stride (x + 1, y + 2) (avg3 i j k)
  writePixel output stride (x + 2, y + 2) (avg2 i j) -- = pred[1][0]
  writePixel output stride (x + 3, y + 2) (avg3 m i j) -- = pred[1][1]
  writePixel output stride (x + 0, y + 3) (avg2 k l)
  writePixel output stride (x + 1, y + 3) (avg3 j k l)
  writePixel output stride (x + 2, y + 3) (avg2 j k) -- = pred[2][0]
  writePixel output stride (x + 3, y + 3) (avg3 i j k) -- = pred[2][1]

predict4x4HU :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s ()
predict4x4HU output stride x y = do
  i <- readPixel output stride (x - 1) (y + 0)
  j <- readPixel output stride (x - 1) (y + 1)
  k <- readPixel output stride (x - 1) (y + 2)
  l <- readPixel output stride (x - 1) (y + 3)

  writePixel output stride (x + 0, y + 0) (avg2 i j)
  writePixel output stride (x + 1, y + 0) (avg3 i j k)
  writePixel output stride (x + 2, y + 0) (avg2 j k)
  writePixel output stride (x + 3, y + 0) (avg3 j k l)

  writePixel output stride (x + 0, y + 1) (avg2 j k)
  writePixel output stride (x + 1, y + 1) (avg3 j k l)
  writePixel output stride (x + 2, y + 1) (avg2 k l)
  writePixel output stride (x + 3, y + 1) (avg3 k l l)

  writePixel output stride (x + 0, y + 2) (avg2 k l)
  writePixel output stride (x + 1, y + 2) (avg3 k l l)
  writePixel output stride (x + 2, y + 2) l
  writePixel output stride (x + 3, y + 2) l

  writePixel output stride (x + 0, y + 3) l
  writePixel output stride (x + 1, y + 3) l
  writePixel output stride (x + 2, y + 3) l
  writePixel output stride (x + 3, y + 3) l

-- Helper functions

-- | Read a reconstructed pixel with RFC 6386 Section 12.2 border semantics:
--
-- * pixels above the frame (y < 0) read as 127 — this includes the
--   above-left pixel of the top-left macroblock,
-- * pixels left of the frame (x < 0, y >= 0) read as 129,
-- * pixels right of the frame replicate the last pixel of their row
--   (no flat-index wrap-around to an adjacent row).
--
-- The buffer is a full plane whose width equals @stride@.
{-# INLINE readPixel #-}
readPixel :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s Word8
readPixel buf stride !px !py
  | py < 0 = return 127
  | px < 0 = return 129
  | otherwise = do
      let !cx = min px (stride - 1)
          !idx = py * stride + cx
      if idx < VSM.length buf
        then VSM.read buf idx
        else return 128

-- | Read above-right pixel @i@ (0..3) of the 4x4 subblock at @(x, y)@, i.e.
-- the pixel logically at @(x+4+i, y-1)@.
--
-- For subblocks in the rightmost column of a macroblock (x mod 16 == 12) the
-- frame buffer at those coordinates belongs to the not-yet-reconstructed
-- right-neighbor MB (subblock rows 1-3) — per the spec/libwebp, ALL four
-- right-column subblocks use the 4 pixels above-right of the macroblock:
-- the bottom row of the above/above-right MBs, which is still intact in the
-- frame buffer at row @(y .&. -16) - 1@. readPixel then supplies the edge
-- behavior: 127 above the top frame row, and replication of the last above
-- pixel past the frame's right edge.
{-# INLINE readTopRight #-}
readTopRight :: VSM.MVector s Word8 -> Int -> Int -> Int -> Int -> ST s Word8
readTopRight buf stride !x !y !i
  | x .&. 15 == 12 = readPixel buf stride (x + 4 + i) ((y .&. complement 15) - 1)
  | otherwise = readPixel buf stride (x + 4 + i) (y - 1)

{-# INLINE writePixel #-}
writePixel :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Word8 -> ST s ()
writePixel buf stride (px, py) val = do
  let idx = py * stride + px
  if idx >= 0 && idx < VSM.length buf
    then VSM.write buf idx val
    else return ()

{-# INLINE sumRow #-}
sumRow :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Int -> ST s Int
sumRow buf stride (startX, startY) len = do
  vals <- mapM (\i -> fromIntegral <$> readPixel buf stride (startX + i) startY) [0 .. len - 1]
  return $ sum vals

{-# INLINE sumCol #-}
sumCol :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Int -> ST s Int
sumCol buf stride (startX, startY) len = do
  vals <- mapM (\i -> fromIntegral <$> readPixel buf stride startX (startY + i)) [0 .. len - 1]
  return $ sum vals

{-# INLINE fillBlock #-}
fillBlock :: VSM.MVector s Word8 -> Int -> (Int, Int) -> Int -> Int -> Int -> ST s ()
fillBlock buf stride (startX, startY) w h val =
  forM_ [0 .. h - 1] $ \row ->
    forM_ [0 .. w - 1] $ \col ->
      writePixel buf stride (startX + col, startY + row) (fromIntegral val)

{-# INLINE clip255 #-}
clip255 :: Int -> Word8
clip255 !x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = fromIntegral x

{-# INLINE avg2 #-}
avg2 :: Word8 -> Word8 -> Word8
avg2 !a !b = fromIntegral ((fromIntegral a + fromIntegral b + 1) `shiftR` 1 :: Int)

{-# INLINE avg3 #-}
avg3 :: Word8 -> Word8 -> Word8 -> Word8
avg3 !a !b !c =
  let !ia = fromIntegral a :: Int
      !ib = fromIntegral b :: Int
      !ic = fromIntegral c :: Int
   in fromIntegral ((ia + 2 * ib + ic + 2) `shiftR` 2)
