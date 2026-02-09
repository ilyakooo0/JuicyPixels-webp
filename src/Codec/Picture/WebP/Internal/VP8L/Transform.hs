{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8L.Transform
  ( VP8LTransform (..),
    applyInverseTransforms,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import Data.STRef
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
  let !totalPixels = width * height
  when (totalPixels < 0 || totalPixels > 100000000) $
    error $
      "Invalid pixel count in subtract green: " ++ show totalPixels

  -- Process in tight loop with unsafeRead/unsafeWrite
  let go !i
        | i >= totalPixels = return ()
        | otherwise = do
            pixel <- VSM.unsafeRead pixels i
            let !g = (pixel `shiftR` 8) .&. 0xFF
                !r = (pixel `shiftR` 16) .&. 0xFF
                !b = pixel .&. 0xFF
                !r' = (r + g) .&. 0xFF
                !b' = (b + g) .&. 0xFF
                !pixel' = (pixel .&. 0xFF00FF00) .|. (r' `shiftL` 16) .|. b'
            VSM.unsafeWrite pixels i pixel'
            go (i + 1)
  go 0

-- | Inverse color transform
inverseColorTransform :: Int -> VS.Vector Word32 -> Int -> Int -> VSM.MVector s Word32 -> ST s ()
inverseColorTransform sizeBits transformData width height pixels = do
  when (sizeBits < 2 || sizeBits > 10) $
    error $
      "Invalid sizeBits in color transform: " ++ show sizeBits

  let !blockSize = 1 `shiftL` sizeBits
      !transformWidth = (width + blockSize - 1) `shiftR` sizeBits

  -- Process row by row with cached transform data lookup
  forM_ [0 .. height - 1] $ \y -> do
    let !transformY = y `shiftR` sizeBits
        !transformRowBase = transformY * transformWidth
        !rowBase = y * width

    forM_ [0 .. width - 1] $ \x -> do
      let !transformX = x `shiftR` sizeBits
          !transformIdx = transformRowBase + transformX
          !transformPixel = VS.unsafeIndex transformData transformIdx

          !greenToRed = toInt8 (fromIntegral ((transformPixel `shiftR` 16) .&. 0xFF) :: Word8)
          !greenToBlue = toInt8 (fromIntegral ((transformPixel `shiftR` 8) .&. 0xFF) :: Word8)
          !redToBlue = toInt8 (fromIntegral (transformPixel .&. 0xFF) :: Word8)

          !idx = rowBase + x

      pixel <- VSM.unsafeRead pixels idx

      let !a = (pixel `shiftR` 24) .&. 0xFF
          !r = fromIntegral ((pixel `shiftR` 16) .&. 0xFF) :: Int
          !g = fromIntegral ((pixel `shiftR` 8) .&. 0xFF) :: Int
          !b = fromIntegral (pixel .&. 0xFF) :: Int
          !gSigned = toInt8 (fromIntegral g :: Word8)

          !tmpRed = (r + colorTransformDelta greenToRed gSigned) .&. 0xFF
          !tmpRedSigned = if tmpRed >= 128 then tmpRed - 256 else tmpRed
          !tmpBlue = (b + colorTransformDelta greenToBlue gSigned + colorTransformDelta redToBlue tmpRedSigned) .&. 0xFF

          !pixel' = (a `shiftL` 24) .|. (fromIntegral tmpRed `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral tmpBlue

      VSM.unsafeWrite pixels idx pixel'

-- | Color transform delta calculation
{-# INLINE colorTransformDelta #-}
colorTransformDelta :: Int -> Int -> Int
colorTransformDelta t c = (t * c) `shiftR` 5

-- | Convert Word8 to signed Int (-128..127)
{-# INLINE toInt8 #-}
toInt8 :: Word8 -> Int
toInt8 w =
  let !i = fromIntegral w :: Int
   in if i >= 128 then i - 256 else i

-- | Inverse predictor transform (optimized with Int arithmetic)
inversePredictorTransform :: Int -> VS.Vector Word32 -> Int -> Int -> VSM.MVector s Word32 -> ST s ()
inversePredictorTransform sizeBits transformData width height pixels = do
  when (sizeBits < 2 || sizeBits > 10) $
    error $
      "Invalid sizeBits in predictor transform: " ++ show sizeBits

  let !blockSize = 1 `shiftL` sizeBits
      !transformWidth = (width + blockSize - 1) `shiftR` sizeBits

  forM_ [0 .. height - 1] $ \y -> do
    let !transformY = y `shiftR` sizeBits
        !transformRowBase = transformY * transformWidth
        !rowBase = y * width
        !prevRowBase = (y - 1) * width
        !isFirstRow = y == 0

    -- Use STRef for left pixel (updated as we scan the row)
    leftRef <- newSTRef (0xFF000000 :: Word32)

    forM_ [0 .. width - 1] $ \x -> do
      let !transformX = x `shiftR` sizeBits
          !transformIdx = transformRowBase + transformX
          !transformPixel = VS.unsafeIndex transformData transformIdx
          !mode = fromIntegral ((transformPixel `shiftR` 8) .&. 0xFF) :: Int
          !idx = rowBase + x

      pixel <- VSM.unsafeRead pixels idx

      -- Read left from STRef (already computed in current row)
      left <- readSTRef leftRef

      -- Read top, topLeft, topRight from pixels array (already computed in previous row)
      -- Using Int arithmetic instead of Integer for index calculation
      top <- if isFirstRow then return 0xFF000000 else VSM.unsafeRead pixels (prevRowBase + x)
      topLeft <- if x == 0 || isFirstRow then return 0xFF000000 else VSM.unsafeRead pixels (prevRowBase + x - 1)
      topRight <- if x >= width - 1 || isFirstRow then return top else VSM.unsafeRead pixels (prevRowBase + x + 1)

      let !predicted = predictor mode left top topLeft topRight
          !result = addPixels pixel predicted

      VSM.unsafeWrite pixels idx result

      -- Update left for next pixel
      writeSTRef leftRef result

-- | Predictor modes
{-# INLINE predictor #-}
predictor :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
predictor 0 _left _top _topLeft _topRight = 0xFF000000
predictor 1 left _top _topLeft _topRight = left
predictor 2 _left top _topLeft _topRight = top
predictor 3 _left _top _topLeft topRight = topRight
predictor 4 _left top topLeft _topRight = top `addPixels` topLeft
predictor 5 left top topLeft _topRight = (left `addPixels` top) `subPixels` topLeft
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
{-# INLINE addPixels #-}
addPixels :: Word32 -> Word32 -> Word32
addPixels p1 p2 =
  let !a1 = (p1 `shiftR` 24) .&. 0xFF
      !r1 = (p1 `shiftR` 16) .&. 0xFF
      !g1 = (p1 `shiftR` 8) .&. 0xFF
      !b1 = p1 .&. 0xFF

      !a2 = (p2 `shiftR` 24) .&. 0xFF
      !r2 = (p2 `shiftR` 16) .&. 0xFF
      !g2 = (p2 `shiftR` 8) .&. 0xFF
      !b2 = p2 .&. 0xFF

      !a = (a1 + a2) .&. 0xFF
      !r = (r1 + r2) .&. 0xFF
      !g = (g1 + g2) .&. 0xFF
      !b = (b1 + b2) .&. 0xFF
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

-- | Subtract two pixels component-wise (mod 256)
{-# INLINE subPixels #-}
subPixels :: Word32 -> Word32 -> Word32
subPixels p1 p2 =
  let !a1 = (p1 `shiftR` 24) .&. 0xFF
      !r1 = (p1 `shiftR` 16) .&. 0xFF
      !g1 = (p1 `shiftR` 8) .&. 0xFF
      !b1 = p1 .&. 0xFF

      !a2 = (p2 `shiftR` 24) .&. 0xFF
      !r2 = (p2 `shiftR` 16) .&. 0xFF
      !g2 = (p2 `shiftR` 8) .&. 0xFF
      !b2 = p2 .&. 0xFF

      !a = (a1 - a2) .&. 0xFF
      !r = (r1 - r2) .&. 0xFF
      !g = (g1 - g2) .&. 0xFF
      !b = (b1 - b2) .&. 0xFF
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

-- | Average of two pixels
{-# INLINE avgPixels2 #-}
avgPixels2 :: Word32 -> Word32 -> Word32
avgPixels2 p1 p2 =
  let !a1 = (p1 `shiftR` 24) .&. 0xFF
      !r1 = (p1 `shiftR` 16) .&. 0xFF
      !g1 = (p1 `shiftR` 8) .&. 0xFF
      !b1 = p1 .&. 0xFF

      !a2 = (p2 `shiftR` 24) .&. 0xFF
      !r2 = (p2 `shiftR` 16) .&. 0xFF
      !g2 = (p2 `shiftR` 8) .&. 0xFF
      !b2 = p2 .&. 0xFF

      !a = (a1 + a2) `shiftR` 1
      !r = (r1 + r2) `shiftR` 1
      !g = (g1 + g2) `shiftR` 1
      !b = (b1 + b2) `shiftR` 1
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

-- | Average of three pixels
{-# INLINE avgPixels3 #-}
avgPixels3 :: Word32 -> Word32 -> Word32 -> Word32
avgPixels3 p1 p2 p3 =
  let !a1 = (p1 `shiftR` 24) .&. 0xFF
      !r1 = (p1 `shiftR` 16) .&. 0xFF
      !g1 = (p1 `shiftR` 8) .&. 0xFF
      !b1 = p1 .&. 0xFF

      !a2 = (p2 `shiftR` 24) .&. 0xFF
      !r2 = (p2 `shiftR` 16) .&. 0xFF
      !g2 = (p2 `shiftR` 8) .&. 0xFF
      !b2 = p2 .&. 0xFF

      !a3 = (p3 `shiftR` 24) .&. 0xFF
      !r3 = (p3 `shiftR` 16) .&. 0xFF
      !g3 = (p3 `shiftR` 8) .&. 0xFF
      !b3 = p3 .&. 0xFF

      !a = (a1 + a2 + a3) `div` 3
      !r = (r1 + r2 + r3) `div` 3
      !g = (g1 + g2 + g3) `div` 3
      !b = (b1 + b2 + b3) `div` 3
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

-- | Select predictor (mode 11)
{-# INLINE select #-}
select :: Word32 -> Word32 -> Word32
select left top =
  let !leftA = (left `shiftR` 24) .&. 0xFF
      !leftR = (left `shiftR` 16) .&. 0xFF
      !leftG = (left `shiftR` 8) .&. 0xFF
      !leftB = left .&. 0xFF

      !topA = (top `shiftR` 24) .&. 0xFF
      !topR = (top `shiftR` 16) .&. 0xFF
      !topG = (top `shiftR` 8) .&. 0xFF
      !topB = top .&. 0xFF

      !pa = abs (fromIntegral topA - fromIntegral leftA :: Int)
      !pr = abs (fromIntegral topR - fromIntegral leftR :: Int)
      !pg = abs (fromIntegral topG - fromIntegral leftG :: Int)
      !pb = abs (fromIntegral topB - fromIntegral leftB :: Int)

      !sum1 = pa + pr + pg + pb
   in if sum1 < 0 then left else top

-- | Clamp add subtract full (mode 12)
{-# INLINE clampAddSubtractFull #-}
clampAddSubtractFull :: Word32 -> Word32 -> Word32 -> Word32
clampAddSubtractFull base delta1 delta2 =
  let !baseA = fromIntegral ((base `shiftR` 24) .&. 0xFF) :: Int
      !baseR = fromIntegral ((base `shiftR` 16) .&. 0xFF) :: Int
      !baseG = fromIntegral ((base `shiftR` 8) .&. 0xFF) :: Int
      !baseB = fromIntegral (base .&. 0xFF) :: Int

      !d1A = fromIntegral ((delta1 `shiftR` 24) .&. 0xFF) :: Int
      !d1R = fromIntegral ((delta1 `shiftR` 16) .&. 0xFF) :: Int
      !d1G = fromIntegral ((delta1 `shiftR` 8) .&. 0xFF) :: Int
      !d1B = fromIntegral (delta1 .&. 0xFF) :: Int

      !d2A = fromIntegral ((delta2 `shiftR` 24) .&. 0xFF) :: Int
      !d2R = fromIntegral ((delta2 `shiftR` 16) .&. 0xFF) :: Int
      !d2G = fromIntegral ((delta2 `shiftR` 8) .&. 0xFF) :: Int
      !d2B = fromIntegral (delta2 .&. 0xFF) :: Int

      !a = clip255Int (baseA + d1A - d2A)
      !r = clip255Int (baseR + d1R - d2R)
      !g = clip255Int (baseG + d1G - d2G)
      !b = clip255Int (baseB + d1B - d2B)
   in (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

-- | Clamp add subtract half (mode 13)
{-# INLINE clampAddSubtractHalf #-}
clampAddSubtractHalf :: Word32 -> Word32 -> Word32 -> Word32
clampAddSubtractHalf base delta1 delta2 =
  let !baseA = fromIntegral ((base `shiftR` 24) .&. 0xFF) :: Int
      !baseR = fromIntegral ((base `shiftR` 16) .&. 0xFF) :: Int
      !baseG = fromIntegral ((base `shiftR` 8) .&. 0xFF) :: Int
      !baseB = fromIntegral (base .&. 0xFF) :: Int

      !d1A = fromIntegral ((delta1 `shiftR` 24) .&. 0xFF) :: Int
      !d1R = fromIntegral ((delta1 `shiftR` 16) .&. 0xFF) :: Int
      !d1G = fromIntegral ((delta1 `shiftR` 8) .&. 0xFF) :: Int
      !d1B = fromIntegral (delta1 .&. 0xFF) :: Int

      !d2A = fromIntegral ((delta2 `shiftR` 24) .&. 0xFF) :: Int
      !d2R = fromIntegral ((delta2 `shiftR` 16) .&. 0xFF) :: Int
      !d2G = fromIntegral ((delta2 `shiftR` 8) .&. 0xFF) :: Int
      !d2B = fromIntegral (delta2 .&. 0xFF) :: Int

      !a = clip255Int (baseA + (d1A - d2A) `shiftR` 1)
      !r = clip255Int (baseR + (d1R - d2R) `shiftR` 1)
      !g = clip255Int (baseG + (d1G - d2G) `shiftR` 1)
      !b = clip255Int (baseB + (d1B - d2B) `shiftR` 1)
   in (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

-- | Clip Int to 0-255 range
{-# INLINE clip255Int #-}
clip255Int :: Int -> Int
clip255Int x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = x

-- | Inverse color indexing transform
inverseColorIndexing :: VS.Vector Word32 -> Int -> Int -> Int -> VSM.MVector s Word32 -> ST s ()
inverseColorIndexing palette widthBits width height pixels = do
  let !totalPixels = width * height
      !paletteLen = VS.length palette

  if widthBits == 0
    then do
      -- Simple case: 1 pixel per byte, direct index lookup
      let go !i
            | i >= totalPixels = return ()
            | otherwise = do
                pixel <- VSM.unsafeRead pixels i
                let !idx = fromIntegral ((pixel `shiftR` 8) .&. 0xFF)
                when (idx < paletteLen) $ do
                  let !color = VS.unsafeIndex palette idx
                  VSM.unsafeWrite pixels i color
                go (i + 1)
      go 0
    else do
      let !bitsPerPixel = 8 `shiftR` widthBits
          !pixelsPerByte = 1 `shiftL` widthBits
          !mask = (1 `shiftL` bitsPerPixel) - 1

      forM_ [0 .. height - 1] $ \y -> do
        let !rowBase = y * width
        forM_ [0 .. width - 1] $ \x -> do
          let !packedIdx = x `shiftR` widthBits
              !subIdx = x .&. (pixelsPerByte - 1)
              !idx = rowBase + packedIdx

          when (idx >= 0 && idx < VSM.length pixels) $ do
            pixel <- VSM.unsafeRead pixels idx
            let !green = (pixel `shiftR` 8) .&. 0xFF
                !shiftAmt = subIdx * bitsPerPixel
                !colorIdx = fromIntegral ((green `shiftR` shiftAmt) .&. mask) :: Int
                !outIdx = rowBase + x

            when (colorIdx < paletteLen && outIdx >= 0 && outIdx < VSM.length pixels) $ do
              let !color = VS.unsafeIndex palette colorIdx
              VSM.unsafeWrite pixels outIdx color
