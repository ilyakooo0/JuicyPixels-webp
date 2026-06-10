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
              if y == 0
                then 0xFF000000
                else
                  if x >= width - 1
                    then pixels VS.! (y * width) -- leftmost pixel of the same row
                    else pixels VS.! (i - width + 1)

            -- Border pixels use fixed predictions regardless of mode (spec Section 4.2.1)
            predicted =
              if x == 0 && y == 0
                then 0xFF000000
                else
                  if y == 0
                    then left
                    else
                      if x == 0
                        then top
                        else predictor mode left top topLeft topRight
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
        | bestSAD == 0 = bestMode -- Can't improve on 0, exit early
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
        if y == 0
          then 0xFF000000
          else
            if x >= width - 1
              then pixels VS.! (y * width)
              else pixels VS.! (i - width + 1)

      -- Border pixels use fixed predictions regardless of mode
      predicted =
        if x == 0 && y == 0
          then 0xFF000000
          else
            if y == 0
              then left
              else
                if x == 0
                  then top
                  else predictor mode left top topLeft topRight
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

-- | Predictor modes (RFC 9649 Section 4.2.1, must match Transform.hs)
{-# INLINE predictor #-}
predictor :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
predictor 0 _left _top _topLeft _topRight = 0xFF000000
predictor 1 left _top _topLeft _topRight = left
predictor 2 _left top _topLeft _topRight = top
predictor 3 _left _top _topLeft topRight = topRight
predictor 4 _left _top topLeft _topRight = topLeft
predictor 5 left top _topLeft topRight = avgPixels2 (avgPixels2 left topRight) top
predictor 6 left _top topLeft _topRight = avgPixels2 left topLeft
predictor 7 left top _topLeft _topRight = avgPixels2 left top
predictor 8 _left top topLeft _topRight = avgPixels2 topLeft top
predictor 9 _left top _topLeft topRight = avgPixels2 top topRight
predictor 10 left top topLeft topRight = avgPixels2 (avgPixels2 left topLeft) (avgPixels2 top topRight)
predictor 11 left top topLeft _topRight = selectPred left top topLeft
predictor 12 left top topLeft _topRight = clampAddSubtractFull left top topLeft
predictor 13 left top topLeft _topRight = clampAddSubtractHalf (avgPixels2 left top) topLeft
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

-- | Select predictor (mode 11, RFC 9649)
{-# INLINE selectPred #-}
selectPred :: Word32 -> Word32 -> Word32 -> Word32
selectPred left top topLeft =
  let lA = fromIntegral ((left `shiftR` 24) .&. 0xFF) :: Int
      lR = fromIntegral ((left `shiftR` 16) .&. 0xFF) :: Int
      lG = fromIntegral ((left `shiftR` 8) .&. 0xFF) :: Int
      lB = fromIntegral (left .&. 0xFF) :: Int

      tA = fromIntegral ((top `shiftR` 24) .&. 0xFF) :: Int
      tR = fromIntegral ((top `shiftR` 16) .&. 0xFF) :: Int
      tG = fromIntegral ((top `shiftR` 8) .&. 0xFF) :: Int
      tB = fromIntegral (top .&. 0xFF) :: Int

      tlA = fromIntegral ((topLeft `shiftR` 24) .&. 0xFF) :: Int
      tlR = fromIntegral ((topLeft `shiftR` 16) .&. 0xFF) :: Int
      tlG = fromIntegral ((topLeft `shiftR` 8) .&. 0xFF) :: Int
      tlB = fromIntegral (topLeft .&. 0xFF) :: Int

      -- ARGB component estimates: p = L + T - TL
      pA = lA + tA - tlA
      pR = lR + tR - tlR
      pG = lG + tG - tlG
      pB = lB + tB - tlB

      -- Manhattan distances to estimates
      pL = abs (pA - lA) + abs (pR - lR) + abs (pG - lG) + abs (pB - lB)
      pT = abs (pA - tA) + abs (pR - tR) + abs (pG - tG) + abs (pB - tB)
   in if pL < pT then left else top

-- | Clamp add subtract full (mode 12): Clamp(a + b - c)
{-# INLINE clampAddSubtractFull #-}
clampAddSubtractFull :: Word32 -> Word32 -> Word32 -> Word32
clampAddSubtractFull pL pT pTL =
  let clip x = if x < 0 then 0 else if x > 255 then 255 else x

      lA = fromIntegral ((pL `shiftR` 24) .&. 0xFF) :: Int
      lR = fromIntegral ((pL `shiftR` 16) .&. 0xFF) :: Int
      lG = fromIntegral ((pL `shiftR` 8) .&. 0xFF) :: Int
      lB = fromIntegral (pL .&. 0xFF) :: Int

      tA = fromIntegral ((pT `shiftR` 24) .&. 0xFF) :: Int
      tR = fromIntegral ((pT `shiftR` 16) .&. 0xFF) :: Int
      tG = fromIntegral ((pT `shiftR` 8) .&. 0xFF) :: Int
      tB = fromIntegral (pT .&. 0xFF) :: Int

      tlA = fromIntegral ((pTL `shiftR` 24) .&. 0xFF) :: Int
      tlR = fromIntegral ((pTL `shiftR` 16) .&. 0xFF) :: Int
      tlG = fromIntegral ((pTL `shiftR` 8) .&. 0xFF) :: Int
      tlB = fromIntegral (pTL .&. 0xFF) :: Int

      a = clip (lA + tA - tlA)
      r = clip (lR + tR - tlR)
      g = clip (lG + tG - tlG)
      b = clip (lB + tB - tlB)
   in (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

-- | Clamp add subtract half (mode 13): Clamp(a + (a - b) / 2)
{-# INLINE clampAddSubtractHalf #-}
clampAddSubtractHalf :: Word32 -> Word32 -> Word32
clampAddSubtractHalf p1 p2 =
  let clip x = if x < 0 then 0 else if x > 255 then 255 else x

      aA = fromIntegral ((p1 `shiftR` 24) .&. 0xFF) :: Int
      aR = fromIntegral ((p1 `shiftR` 16) .&. 0xFF) :: Int
      aG = fromIntegral ((p1 `shiftR` 8) .&. 0xFF) :: Int
      aB = fromIntegral (p1 .&. 0xFF) :: Int

      bA = fromIntegral ((p2 `shiftR` 24) .&. 0xFF) :: Int
      bR = fromIntegral ((p2 `shiftR` 16) .&. 0xFF) :: Int
      bG = fromIntegral ((p2 `shiftR` 8) .&. 0xFF) :: Int
      bB = fromIntegral (p2 .&. 0xFF) :: Int

      a = clip (aA + (aA - bA) `quot` 2)
      r = clip (aR + (aR - bR) `quot` 2)
      g = clip (aG + (aG - bG) `quot` 2)
      b = clip (aB + (aB - bB) `quot` 2)
   in (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b
