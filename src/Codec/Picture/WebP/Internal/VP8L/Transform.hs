{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8L.Transform
  ( VP8LTransform (..),
    applyInverseTransforms,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word

-- | VP8L transforms
data VP8LTransform
  = TransformPredictor !Int !(VS.Vector Word32)
  | TransformColor !Int !(VS.Vector Word32)
  | TransformSubGreen
  | TransformColorIndex !(VS.Vector Word32) !Int
  deriving (Show)

-- | Apply inverse transforms in reverse order
applyInverseTransforms :: [VP8LTransform] -> Int -> Int -> VS.Vector Word32 -> Either String (VS.Vector Word32)
applyInverseTransforms transforms width height pixels =
  runST $ do
    mutablePixels <- VS.thaw pixels
    mapM_ (\t -> applyInverseTransform t width height mutablePixels) (reverse transforms)
    result <- VS.unsafeFreeze mutablePixels
    return $ Right result

-- | Apply a single inverse transform
applyInverseTransform :: VP8LTransform -> Int -> Int -> VSM.MVector s Word32 -> ST s ()
applyInverseTransform TransformSubGreen width height pixels =
  inverseSubtractGreen width height pixels
applyInverseTransform (TransformColor sizeBits transformData) width height pixels =
  inverseColorTransform sizeBits transformData width height pixels
applyInverseTransform (TransformPredictor sizeBits transformData) width height pixels =
  inversePredictorTransform sizeBits transformData width height pixels
applyInverseTransform (TransformColorIndex palette widthBits) width height pixels =
  inverseColorIndexing palette widthBits width height pixels

-- | Inverse subtract green transform
inverseSubtractGreen :: Int -> Int -> VSM.MVector s Word32 -> ST s ()
inverseSubtractGreen width height pixels = do
  let totalPixels = width * height
  when (totalPixels < 0 || totalPixels > 100000000) $
    error $ "Invalid pixel count in subtract green: " ++ show totalPixels

  forM_ [0 .. totalPixels - 1] $ \i -> do
    pixel <- VSM.read pixels i
    let r = fromIntegral ((pixel `shiftR` 16) .&. 0xFF) :: Word8
        g = fromIntegral ((pixel `shiftR` 8) .&. 0xFF) :: Word8
        b = fromIntegral (pixel .&. 0xFF) :: Word8

        r' = (r + g) .&. 0xFF
        b' = (b + g) .&. 0xFF

        pixel' = (pixel .&. 0xFF00FF00) .|. (fromIntegral r' `shiftL` 16) .|. fromIntegral b'

    VSM.write pixels i pixel'

-- | Inverse color transform
inverseColorTransform :: Int -> VS.Vector Word32 -> Int -> Int -> VSM.MVector s Word32 -> ST s ()
inverseColorTransform sizeBits transformData width height pixels = do
  when (sizeBits < 2 || sizeBits > 10) $
    error $ "Invalid sizeBits in color transform: " ++ show sizeBits

  let blockSize = 1 `shiftL` sizeBits
      transformWidth = (width + blockSize - 1) `shiftR` sizeBits

  forM_ [0 .. height - 1] $ \y -> do
    forM_ [0 .. width - 1] $ \x -> do
      let transformX = x `shiftR` sizeBits
          transformY = y `shiftR` sizeBits
          -- Use Integer to avoid overflow
          transformIdxInteger = (fromIntegral transformY :: Integer) * (fromIntegral transformWidth :: Integer) + (fromIntegral transformX :: Integer)
          transformIdx = fromIntegral transformIdxInteger :: Int
          transformPixel = transformData VS.! transformIdx

          greenToRed = toInt8 (fromIntegral ((transformPixel `shiftR` 16) .&. 0xFF) :: Word8)
          greenToBlue = toInt8 (fromIntegral ((transformPixel `shiftR` 8) .&. 0xFF) :: Word8)
          redToBlue = toInt8 (fromIntegral (transformPixel .&. 0xFF) :: Word8)

          idxInteger = (fromIntegral y :: Integer) * (fromIntegral width :: Integer) + (fromIntegral x :: Integer)
          idx = fromIntegral idxInteger :: Int

      pixel <- VSM.read pixels idx

      let a = fromIntegral ((pixel `shiftR` 24) .&. 0xFF) :: Word8
          r = fromIntegral ((pixel `shiftR` 16) .&. 0xFF) :: Word8
          g = fromIntegral ((pixel `shiftR` 8) .&. 0xFF) :: Word8
          b = fromIntegral (pixel .&. 0xFF) :: Word8

          tmpRed = (fromIntegral r + colorTransformDelta greenToRed (toInt8 g)) .&. 0xFF
          tmpBlue = (fromIntegral b + colorTransformDelta greenToBlue (toInt8 g) + colorTransformDelta redToBlue (fromIntegral tmpRed - (if tmpRed >= 128 then 256 else 0))) .&. 0xFF

          pixel' = (fromIntegral a `shiftL` 24) .|. (fromIntegral tmpRed `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral tmpBlue

      VSM.write pixels idx pixel'

-- | Color transform delta calculation
colorTransformDelta :: Int -> Int -> Int
colorTransformDelta t c = (t * c) `shiftR` 5

-- | Convert Word8 to signed Int (-128..127)
toInt8 :: Word8 -> Int
toInt8 w =
  let i = fromIntegral w :: Int
   in if i >= 128 then i - 256 else i

-- | Inverse predictor transform
inversePredictorTransform :: Int -> VS.Vector Word32 -> Int -> Int -> VSM.MVector s Word32 -> ST s ()
inversePredictorTransform sizeBits transformData width height pixels = do
  when (sizeBits < 2 || sizeBits > 10) $
    error $ "Invalid sizeBits in predictor transform: " ++ show sizeBits

  let blockSize = 1 `shiftL` sizeBits
      transformWidth = (width + blockSize - 1) `shiftR` sizeBits

  forM_ [0 .. height - 1] $ \y -> do
    forM_ [0 .. width - 1] $ \x -> do
      let transformX = x `shiftR` sizeBits
          transformY = y `shiftR` sizeBits
          -- Use Integer to avoid overflow
          transformIdxInteger = (fromIntegral transformY :: Integer) * (fromIntegral transformWidth :: Integer) + (fromIntegral transformX :: Integer)
          transformIdx = fromIntegral transformIdxInteger :: Int
          transformPixel = transformData VS.! transformIdx
          mode = fromIntegral ((transformPixel `shiftR` 8) .&. 0xFF) :: Int

          idxInteger = (fromIntegral y :: Integer) * (fromIntegral width :: Integer) + (fromIntegral x :: Integer)
          idx = fromIntegral idxInteger :: Int

      pixel <- VSM.read pixels idx

      left <- if x > 0 then VSM.read pixels (idx - 1) else return 0xFF000000
      top <- if y > 0 then do
                let topIdxInteger = (fromIntegral (y - 1) :: Integer) * (fromIntegral width :: Integer) + (fromIntegral x :: Integer)
                    topIdx = fromIntegral topIdxInteger :: Int
                VSM.read pixels topIdx
             else return 0xFF000000
      topLeft <- if x > 0 && y > 0 then do
                   let tlIdxInteger = (fromIntegral (y - 1) :: Integer) * (fromIntegral width :: Integer) + (fromIntegral (x - 1) :: Integer)
                       tlIdx = fromIntegral tlIdxInteger :: Int
                   VSM.read pixels tlIdx
                 else return 0xFF000000
      topRight <- if x < width - 1 && y > 0 then do
                    let trIdxInteger = (fromIntegral (y - 1) :: Integer) * (fromIntegral width :: Integer) + (fromIntegral (x + 1) :: Integer)
                        trIdx = fromIntegral trIdxInteger :: Int
                    VSM.read pixels trIdx
                  else return top

      let predicted = predictor mode left top topLeft topRight
          result = addPixels pixel predicted

      VSM.write pixels idx result

-- | Predictor modes
predictor :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
predictor 0 _left _top _topLeft _topRight = 0xFF000000
predictor 1 left _top _topLeft _topRight = left
predictor 2 _left top _topLeft _topRight = top
predictor 3 _left _top _topLeft topRight = topRight
predictor 4 _left top topLeft _topRight = top `addPixels` topLeft
predictor 5 left top topLeft _topRight =
  let pred = (left `addPixels` top) `subPixels` topLeft
   in pred
predictor 6 left _top _topLeft _topRight = left
predictor 7 _left top _topLeft _topRight = top
predictor 8 _left _top topLeft _topRight = topLeft
predictor 9 left top _topLeft _topRight = avgPixels2 left top
predictor 10 left top topLeft _topRight = avgPixels3 left top topLeft
predictor 11 left top _topLeft _topRight = select left top
predictor 12 left _top topLeft _topRight = clampAddSubtractFull left topLeft topLeft
predictor 13 left top topLeft _topRight = clampAddSubtractHalf left top topLeft
predictor _ _left _top _topLeft _topRight = 0xFF000000

-- | Add two pixels component-wise (mod 256)
addPixels :: Word32 -> Word32 -> Word32
addPixels p1 p2 =
  let a1 = (p1 `shiftR` 24) .&. 0xFF
      r1 = (p1 `shiftR` 16) .&. 0xFF
      g1 = (p1 `shiftR` 8) .&. 0xFF
      b1 = p1 .&. 0xFF

      a2 = (p2 `shiftR` 24) .&. 0xFF
      r2 = (p2 `shiftR` 16) .&. 0xFF
      g2 = (p2 `shiftR` 8) .&. 0xFF
      b2 = p2 .&. 0xFF

      a = (a1 + a2) .&. 0xFF
      r = (r1 + r2) .&. 0xFF
      g = (g1 + g2) .&. 0xFF
      b = (b1 + b2) .&. 0xFF
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

-- | Subtract two pixels component-wise (mod 256)
subPixels :: Word32 -> Word32 -> Word32
subPixels p1 p2 =
  let a1 = (p1 `shiftR` 24) .&. 0xFF
      r1 = (p1 `shiftR` 16) .&. 0xFF
      g1 = (p1 `shiftR` 8) .&. 0xFF
      b1 = p1 .&. 0xFF

      a2 = (p2 `shiftR` 24) .&. 0xFF
      r2 = (p2 `shiftR` 16) .&. 0xFF
      g2 = (p2 `shiftR` 8) .&. 0xFF
      b2 = p2 .&. 0xFF

      a = (a1 - a2) .&. 0xFF
      r = (r1 - r2) .&. 0xFF
      g = (g1 - g2) .&. 0xFF
      b = (b1 - b2) .&. 0xFF
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

-- | Average of two pixels
avgPixels2 :: Word32 -> Word32 -> Word32
avgPixels2 p1 p2 =
  let a1 = (p1 `shiftR` 24) .&. 0xFF
      r1 = (p1 `shiftR` 16) .&. 0xFF
      g1 = (p1 `shiftR` 8) .&. 0xFF
      b1 = p1 .&. 0xFF

      a2 = (p2 `shiftR` 24) .&. 0xFF
      r2 = (p2 `shiftR` 16) .&. 0xFF
      g2 = (p2 `shiftR` 8) .&. 0xFF
      b2 = p2 .&. 0xFF

      a = (a1 + a2) `shiftR` 1
      r = (r1 + r2) `shiftR` 1
      g = (g1 + g2) `shiftR` 1
      b = (b1 + b2) `shiftR` 1
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

-- | Average of three pixels
avgPixels3 :: Word32 -> Word32 -> Word32 -> Word32
avgPixels3 p1 p2 p3 =
  let a1 = (p1 `shiftR` 24) .&. 0xFF
      r1 = (p1 `shiftR` 16) .&. 0xFF
      g1 = (p1 `shiftR` 8) .&. 0xFF
      b1 = p1 .&. 0xFF

      a2 = (p2 `shiftR` 24) .&. 0xFF
      r2 = (p2 `shiftR` 16) .&. 0xFF
      g2 = (p2 `shiftR` 8) .&. 0xFF
      b2 = p2 .&. 0xFF

      a3 = (p3 `shiftR` 24) .&. 0xFF
      r3 = (p3 `shiftR` 16) .&. 0xFF
      g3 = (p3 `shiftR` 8) .&. 0xFF
      b3 = p3 .&. 0xFF

      a = (a1 + a2 + a3) `div` 3
      r = (r1 + r2 + r3) `div` 3
      g = (g1 + g2 + g3) `div` 3
      b = (b1 + b2 + b3) `div` 3
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

-- | Select predictor (mode 11)
select :: Word32 -> Word32 -> Word32
select left top =
  let leftA = (left `shiftR` 24) .&. 0xFF
      leftR = (left `shiftR` 16) .&. 0xFF
      leftG = (left `shiftR` 8) .&. 0xFF
      leftB = left .&. 0xFF

      topA = (top `shiftR` 24) .&. 0xFF
      topR = (top `shiftR` 16) .&. 0xFF
      topG = (top `shiftR` 8) .&. 0xFF
      topB = top .&. 0xFF

      pa = abs (fromIntegral topA - fromIntegral leftA) :: Int
      pr = abs (fromIntegral topR - fromIntegral leftR) :: Int
      pg = abs (fromIntegral topG - fromIntegral leftG) :: Int
      pb = abs (fromIntegral topB - fromIntegral leftB) :: Int

      sum1 = pa + pr + pg + pb
      sum2 = 0
   in if sum1 < sum2 then left else top

-- | Clamp add subtract full (mode 12)
clampAddSubtractFull :: Word32 -> Word32 -> Word32 -> Word32
clampAddSubtractFull base delta1 delta2 =
  let clip x = if x < 0 then 0 else if x > 255 then 255 else x

      baseA = fromIntegral ((base `shiftR` 24) .&. 0xFF) :: Int
      baseR = fromIntegral ((base `shiftR` 16) .&. 0xFF) :: Int
      baseG = fromIntegral ((base `shiftR` 8) .&. 0xFF) :: Int
      baseB = fromIntegral (base .&. 0xFF) :: Int

      d1A = fromIntegral ((delta1 `shiftR` 24) .&. 0xFF) :: Int
      d1R = fromIntegral ((delta1 `shiftR` 16) .&. 0xFF) :: Int
      d1G = fromIntegral ((delta1 `shiftR` 8) .&. 0xFF) :: Int
      d1B = fromIntegral (delta1 .&. 0xFF) :: Int

      d2A = fromIntegral ((delta2 `shiftR` 24) .&. 0xFF) :: Int
      d2R = fromIntegral ((delta2 `shiftR` 16) .&. 0xFF) :: Int
      d2G = fromIntegral ((delta2 `shiftR` 8) .&. 0xFF) :: Int
      d2B = fromIntegral (delta2 .&. 0xFF) :: Int

      a = clip (baseA + d1A - d2A)
      r = clip (baseR + d1R - d2R)
      g = clip (baseG + d1G - d2G)
      b = clip (baseB + d1B - d2B)
   in (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

-- | Clamp add subtract half (mode 13)
clampAddSubtractHalf :: Word32 -> Word32 -> Word32 -> Word32
clampAddSubtractHalf base delta1 delta2 =
  let clip x = if x < 0 then 0 else if x > 255 then 255 else x

      baseA = fromIntegral ((base `shiftR` 24) .&. 0xFF) :: Int
      baseR = fromIntegral ((base `shiftR` 16) .&. 0xFF) :: Int
      baseG = fromIntegral ((base `shiftR` 8) .&. 0xFF) :: Int
      baseB = fromIntegral (base .&. 0xFF) :: Int

      d1A = fromIntegral ((delta1 `shiftR` 24) .&. 0xFF) :: Int
      d1R = fromIntegral ((delta1 `shiftR` 16) .&. 0xFF) :: Int
      d1G = fromIntegral ((delta1 `shiftR` 8) .&. 0xFF) :: Int
      d1B = fromIntegral (delta1 .&. 0xFF) :: Int

      d2A = fromIntegral ((delta2 `shiftR` 24) .&. 0xFF) :: Int
      d2R = fromIntegral ((delta2 `shiftR` 16) .&. 0xFF) :: Int
      d2G = fromIntegral ((delta2 `shiftR` 8) .&. 0xFF) :: Int
      d2B = fromIntegral (delta2 .&. 0xFF) :: Int

      a = clip (baseA + (d1A - d2A) `div` 2)
      r = clip (baseR + (d1R - d2R) `div` 2)
      g = clip (baseG + (d1G - d2G) `div` 2)
      b = clip (baseB + (d1B - d2B) `div` 2)
   in (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

-- | Inverse color indexing transform
inverseColorIndexing :: VS.Vector Word32 -> Int -> Int -> Int -> VSM.MVector s Word32 -> ST s ()
inverseColorIndexing palette widthBits width height pixels = do
  let totalPixels = width * height

  if widthBits == 0
    then forM_ [0 .. totalPixels - 1] $ \i -> do
      pixel <- VSM.read pixels i
      let idx = fromIntegral ((pixel `shiftR` 8) .&. 0xFF)
      when (idx < VS.length palette) $ do
        let color = palette VS.! idx
        VSM.write pixels i color
    else do
      let bitsPerPixel = 8 `shiftR` widthBits
          pixelsPerByte = 1 `shiftL` widthBits
          mask = (1 `shiftL` bitsPerPixel) - 1

      forM_ [0 .. height - 1] $ \y -> do
        forM_ [0 .. width - 1] $ \x -> do
          let packedIdx = x `shiftR` widthBits
              subIdx = x .&. (pixelsPerByte - 1)
              -- Use Integer to avoid overflow for large images
              idxInteger = (fromIntegral y :: Integer) * (fromIntegral width :: Integer) + (fromIntegral packedIdx :: Integer)
              idx = fromIntegral idxInteger :: Int

          when (idx >= 0 && idx < VSM.length pixels) $ do
            pixel <- VSM.read pixels idx
            let green = (pixel `shiftR` 8) .&. 0xFF
                shiftAmt = subIdx * bitsPerPixel
                colorIdx = if shiftAmt > 20
                             then error $ "Bit shift overflow: subIdx=" ++ show subIdx ++ ", bitsPerPixel=" ++ show bitsPerPixel
                             else (green `shiftR` shiftAmt) .&. mask
                outIdxInteger = (fromIntegral y :: Integer) * (fromIntegral width :: Integer) + (fromIntegral x :: Integer)
                outIdx = fromIntegral outIdxInteger :: Int

            when (fromIntegral colorIdx < VS.length palette && outIdx >= 0 && outIdx < VSM.length pixels) $ do
              let color = palette VS.! fromIntegral colorIdx
              VSM.write pixels outIdx color
