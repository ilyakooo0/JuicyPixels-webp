{-# LANGUAGE BangPatterns #-}

-- | VP8L predictor transform encoder
module Codec.Picture.WebP.Internal.VP8L.PredictorEncode
  ( PredictorResult (..),
    computePredictorTransform,
  )
where

import Data.Bits
import qualified Data.Vector.Storable as VS
import Data.Word

-- | Result of computing the predictor transform
data PredictorResult = PredictorResult
  { -- | Size bits (2-10), block size = 2^sizeBits
    prSizeBits :: !Int,
    -- | Mode image - mode stored in green channel (bits 8-15)
    prModeImage :: !(VS.Vector Word32),
    -- | Residual pixels (original - predicted)
    prResiduals :: !(VS.Vector Word32),
    -- | Width of the mode/transform image
    prTransformWidth :: !Int,
    -- | Height of the mode/transform image
    prTransformHeight :: !Int
  }
  deriving (Show)

-- | Compute the predictor transform for an image
-- sizeBits: 2-10, determines block size (2^sizeBits)
computePredictorTransform :: Int -> Int -> Int -> VS.Vector Word32 -> PredictorResult
computePredictorTransform sizeBits width height pixels =
  let blockSize = 1 `shiftL` sizeBits
      transformWidth = (width + blockSize - 1) `shiftR` sizeBits
      transformHeight = (height + blockSize - 1) `shiftR` sizeBits

      -- Compute best mode for each block
      modeImage = VS.generate (transformWidth * transformHeight) $ \i ->
        let bx = i `mod` transformWidth
            by = i `div` transformWidth
            mode = selectBestMode sizeBits bx by width height pixels
         in -- Mode is stored in the green channel (bits 8-15)
            fromIntegral mode `shiftL` 8

      -- Compute residuals for all pixels
      residuals = VS.generate (width * height) $ \i ->
        let x = i `mod` width
            y = i `div` width
            pixel = pixels VS.! i

            -- Get mode for this block
            bx = x `shiftR` sizeBits
            by = y `shiftR` sizeBits
            modePixel = modeImage VS.! (by * transformWidth + bx)
            mode = fromIntegral ((modePixel `shiftR` 8) .&. 0xFF) :: Int

            -- Get neighbor pixels (must match decoder exactly)
            left = if x > 0 then pixels VS.! (i - 1) else 0xFF000000
            top = if y > 0 then pixels VS.! (i - width) else 0xFF000000
            topLeft =
              if x > 0 && y > 0
                then pixels VS.! (i - width - 1)
                else 0xFF000000
            topRight =
              if x < width - 1 && y > 0
                then pixels VS.! (i - width + 1)
                else top -- Same as decoder line 161

            predicted = predictor mode left top topLeft topRight
         in subPixels pixel predicted
   in PredictorResult
        { prSizeBits = sizeBits,
          prModeImage = modeImage,
          prResiduals = residuals,
          prTransformWidth = transformWidth,
          prTransformHeight = transformHeight
        }

-- | Select the best prediction mode for a block using SAD (Sum of Absolute Differences)
-- Optimized with early exit when SAD = 0 (perfect match)
{-# INLINE selectBestMode #-}
selectBestMode :: Int -> Int -> Int -> Int -> Int -> VS.Vector Word32 -> Int
selectBestMode sizeBits bx by width height pixels =
  let !blockSize = 1 `shiftL` sizeBits
      !startX = bx * blockSize
      !startY = by * blockSize
      !endX = min (startX + blockSize) width
      !endY = min (startY + blockSize) height

      -- Strict fold with early exit when SAD = 0
      go !bestMode !bestSAD !mode
        | mode > 13 = bestMode
        | bestSAD == 0 = bestMode  -- Can't improve on 0, exit early
        | otherwise =
            let !sad = computeBlockSAD startX startY endX endY width height pixels mode
             in if sad < bestSAD
                  then go mode sad (mode + 1)
                  else go bestMode bestSAD (mode + 1)

      -- Start with mode 0
      !initialSAD = computeBlockSAD startX startY endX endY width height pixels 0
   in go 0 initialSAD 1

-- | Compute SAD for a block with a given mode
{-# INLINE computeBlockSAD #-}
computeBlockSAD :: Int -> Int -> Int -> Int -> Int -> Int -> VS.Vector Word32 -> Int -> Int
computeBlockSAD startX startY endX endY width height pixels mode =
  let coords = [(x, y) | y <- [startY .. endY - 1], x <- [startX .. endX - 1]]
   in sum $ map (pixelSAD width height pixels mode) coords

-- | Compute SAD for a single pixel with a given mode
{-# INLINE pixelSAD #-}
pixelSAD :: Int -> Int -> VS.Vector Word32 -> Int -> (Int, Int) -> Int
pixelSAD width _height pixels mode (x, y) =
  let i = y * width + x
      pixel = pixels VS.! i

      -- Get neighbor pixels (must match decoder exactly)
      left = if x > 0 then pixels VS.! (i - 1) else 0xFF000000
      top = if y > 0 then pixels VS.! (i - width) else 0xFF000000
      topLeft =
        if x > 0 && y > 0
          then pixels VS.! (i - width - 1)
          else 0xFF000000
      topRight =
        if x < width - 1 && y > 0
          then pixels VS.! (i - width + 1)
          else top

      predicted = predictor mode left top topLeft topRight
      residual = subPixels pixel predicted

      -- Extract components and sum absolute differences
      a = fromIntegral ((residual `shiftR` 24) .&. 0xFF) :: Int
      r = fromIntegral ((residual `shiftR` 16) .&. 0xFF) :: Int
      g = fromIntegral ((residual `shiftR` 8) .&. 0xFF) :: Int
      b = fromIntegral (residual .&. 0xFF) :: Int

      -- Convert to signed and take absolute value
      -- Residuals are mod 256, so values > 127 are negative
      signedAbs v = if v > 127 then 256 - v else v
   in signedAbs a + signedAbs r + signedAbs g + signedAbs b

-- | Predictor modes (same as Transform.hs)
{-# INLINE predictor #-}
predictor :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
predictor 0 _left _top _topLeft _topRight = 0xFF000000
predictor 1 left _top _topLeft _topRight = left
predictor 2 _left top _topLeft _topRight = top
predictor 3 _left _top _topLeft topRight = topRight
predictor 4 _left top topLeft _topRight = top `addPixels` topLeft
predictor 5 left top topLeft _topRight =
  (left `addPixels` top) `subPixels` topLeft
predictor 6 left _top _topLeft _topRight = left
predictor 7 _left top _topLeft _topRight = top
predictor 8 _left _top topLeft _topRight = topLeft
predictor 9 left top _topLeft _topRight = avgPixels2 left top
predictor 10 left top topLeft _topRight = avgPixels3 left top topLeft
predictor 11 left top _topLeft _topRight = selectPredictor left top
predictor 12 left _top topLeft _topRight = clampAddSubtractFull left topLeft topLeft
predictor 13 left top topLeft _topRight = clampAddSubtractHalf left top topLeft
predictor _ _left _top _topLeft _topRight = 0xFF000000

-- | Add two pixels component-wise (mod 256)
{-# INLINE addPixels #-}
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
{-# INLINE subPixels #-}
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
{-# INLINE avgPixels2 #-}
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
{-# INLINE avgPixels3 #-}
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
selectPredictor :: Word32 -> Word32 -> Word32
selectPredictor left top =
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

      -- sum1 is the distance from top, sum2 from left (both 0 in original code)
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
