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

-- | Apply inverse transforms in reverse order.
-- Color-indexing is handled separately because it may change image dimensions (pixel bundling).
applyInverseTransforms :: [VP8LTransform] -> Int -> Int -> VS.Vector Word32 -> Either String (VS.Vector Word32)
applyInverseTransforms transforms origWidth height pixels = do
  let -- Separate color-indexing from other transforms
      (maybeCI, otherTransforms) = extractColorIndex transforms

      isColorIndex t = case t of
        TransformColorIndex _ _ -> True
        _ -> False

  -- Inverse transforms are applied in reverse read order; pulling color-indexing
  -- to the end of the inverse chain is only valid when it was read first.
  case (maybeCI, transforms) of
    (Just _, t : _)
      | not (isColorIndex t) ->
          Left "Unsupported transform order: color-indexing transform is not the first transform"
    _ -> return ()

  let -- Width used for in-place transforms (packed width if bundling)
      effectiveWidth = case maybeCI of
        Just (TransformColorIndex _ wb)
          | wb > 0 ->
              (origWidth + (1 `shiftL` wb) - 1) `shiftR` wb
        _ -> origWidth

      -- Apply non-color-indexing inverse transforms in-place (reverse order)
      afterInPlace = runST $ do
        mp <- VS.thaw pixels
        mapM_ (\t -> applyInPlaceInverseTransform t effectiveWidth height mp) (reverse otherTransforms)
        VS.unsafeFreeze mp

  -- Apply color-indexing inverse (creates new vector at original dimensions)
  case maybeCI of
    Just (TransformColorIndex palette widthBits) ->
      Right $ inverseColorIndexingPure palette widthBits origWidth height afterInPlace
    _ -> Right afterInPlace

-- | Extract the color-indexing transform (if any) from the list.
-- Returns (color-indexing transform, remaining transforms in original order).
extractColorIndex :: [VP8LTransform] -> (Maybe VP8LTransform, [VP8LTransform])
extractColorIndex = go Nothing []
  where
    go ci acc [] = (ci, reverse acc)
    go _ acc (t@(TransformColorIndex _ _) : rest) = go (Just t) acc rest
    go ci acc (t : rest) = go ci (t : acc) rest

-- | Apply a single in-place inverse transform (everything except color-indexing)
applyInPlaceInverseTransform :: VP8LTransform -> Int -> Int -> VSM.MVector s Word32 -> ST s ()
applyInPlaceInverseTransform TransformSubGreen width height pixels =
  inverseSubtractGreen width height pixels
applyInPlaceInverseTransform (TransformColor sizeBits transformData) width height pixels =
  inverseColorTransform sizeBits transformData width height pixels
applyInPlaceInverseTransform (TransformPredictor sizeBits transformData) width height pixels =
  inversePredictorTransform sizeBits transformData width height pixels
applyInPlaceInverseTransform (TransformColorIndex _ _) _ _ _ =
  return () -- Handled separately in applyInverseTransforms

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

          -- Spec: alpha=255, red=red_to_blue, green=green_to_blue, blue=green_to_red
          !greenToRed = toInt8 (fromIntegral (transformPixel .&. 0xFF) :: Word8)
          !greenToBlue = toInt8 (fromIntegral ((transformPixel `shiftR` 8) .&. 0xFF) :: Word8)
          !redToBlue = toInt8 (fromIntegral ((transformPixel `shiftR` 16) .&. 0xFF) :: Word8)

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
      topRight <-
        if isFirstRow
          then return 0xFF000000
          else
            if x >= width - 1
              then VSM.unsafeRead pixels rowBase -- leftmost pixel of current row
              else VSM.unsafeRead pixels (prevRowBase + x + 1)

      -- Border pixels use fixed predictions regardless of mode (spec Section 4.2.1)
      let !predicted =
            if x == 0 && isFirstRow
              then 0xFF000000
              else
                if isFirstRow
                  then left
                  else
                    if x == 0
                      then top
                      else predictor mode left top topLeft topRight
          !result = addPixels pixel predicted

      VSM.unsafeWrite pixels idx result

      -- Update left for next pixel
      writeSTRef leftRef result

-- | Predictor modes (RFC 9649 Section 4.2.1)
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
predictor 11 left top topLeft _topRight = selectPredictor left top topLeft
predictor 12 left top topLeft _topRight = clampAddSubtractFull left top topLeft
predictor 13 left top topLeft _topRight = clampAddSubtractHalf (avgPixels2 left top) topLeft
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

-- | Select predictor (mode 11, RFC 9649)
{-# INLINE selectPredictor #-}
selectPredictor :: Word32 -> Word32 -> Word32 -> Word32
selectPredictor left top topLeft =
  let !lA = fromIntegral ((left `shiftR` 24) .&. 0xFF) :: Int
      !lR = fromIntegral ((left `shiftR` 16) .&. 0xFF) :: Int
      !lG = fromIntegral ((left `shiftR` 8) .&. 0xFF) :: Int
      !lB = fromIntegral (left .&. 0xFF) :: Int

      !tA = fromIntegral ((top `shiftR` 24) .&. 0xFF) :: Int
      !tR = fromIntegral ((top `shiftR` 16) .&. 0xFF) :: Int
      !tG = fromIntegral ((top `shiftR` 8) .&. 0xFF) :: Int
      !tB = fromIntegral (top .&. 0xFF) :: Int

      !tlA = fromIntegral ((topLeft `shiftR` 24) .&. 0xFF) :: Int
      !tlR = fromIntegral ((topLeft `shiftR` 16) .&. 0xFF) :: Int
      !tlG = fromIntegral ((topLeft `shiftR` 8) .&. 0xFF) :: Int
      !tlB = fromIntegral (topLeft .&. 0xFF) :: Int

      -- ARGB component estimates: p = L + T - TL
      !pA = lA + tA - tlA
      !pR = lR + tR - tlR
      !pG = lG + tG - tlG
      !pB = lB + tB - tlB

      -- Manhattan distances to estimates
      !pL = abs (pA - lA) + abs (pR - lR) + abs (pG - lG) + abs (pB - lB)
      !pT = abs (pA - tA) + abs (pR - tR) + abs (pG - tG) + abs (pB - tB)
   in if pL < pT then left else top

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

-- | Clamp add subtract half (mode 13): Clamp(a + (a - b) / 2)
{-# INLINE clampAddSubtractHalf #-}
clampAddSubtractHalf :: Word32 -> Word32 -> Word32
clampAddSubtractHalf p1 p2 =
  let !aA = fromIntegral ((p1 `shiftR` 24) .&. 0xFF) :: Int
      !aR = fromIntegral ((p1 `shiftR` 16) .&. 0xFF) :: Int
      !aG = fromIntegral ((p1 `shiftR` 8) .&. 0xFF) :: Int
      !aB = fromIntegral (p1 .&. 0xFF) :: Int

      !bA = fromIntegral ((p2 `shiftR` 24) .&. 0xFF) :: Int
      !bR = fromIntegral ((p2 `shiftR` 16) .&. 0xFF) :: Int
      !bG = fromIntegral ((p2 `shiftR` 8) .&. 0xFF) :: Int
      !bB = fromIntegral (p2 .&. 0xFF) :: Int

      !a = clip255Int (aA + (aA - bA) `quot` 2)
      !r = clip255Int (aR + (aR - bR) `quot` 2)
      !g = clip255Int (aG + (aG - bG) `quot` 2)
      !b = clip255Int (aB + (aB - bB) `quot` 2)
   in (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

-- | Clip Int to 0-255 range
{-# INLINE clip255Int #-}
clip255Int :: Int -> Int
clip255Int x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = x

-- | Pure inverse color-indexing transform. Creates a new vector at the original dimensions.
-- Handles both the simple case (widthBits=0, no bundling) and the bundled case.
inverseColorIndexingPure :: VS.Vector Word32 -> Int -> Int -> Int -> VS.Vector Word32 -> VS.Vector Word32
inverseColorIndexingPure palette widthBits origWidth height packedPixels
  | widthBits == 0 =
      -- No bundling: direct 1:1 index replacement
      VS.generate (origWidth * height) $ \i ->
        let !px = packedPixels `VS.unsafeIndex` i
            !idx = fromIntegral ((px `shiftR` 8) .&. 0xFF)
         in if idx < paletteLen then palette `VS.unsafeIndex` idx else 0x00000000
  | otherwise =
      -- With bundling: expand packed pixels to original width
      let !bpp = 8 `shiftR` widthBits
          !ppb = 1 `shiftL` widthBits
          !mask = (1 `shiftL` bpp) - 1 :: Word32
          !packedWidth = VS.length packedPixels `div` max 1 height
       in VS.generate (origWidth * height) $ \i ->
            let !y = i `div` origWidth
                !x = i `mod` origWidth
                !packedX = x `shiftR` widthBits
                !subIdx = x .&. (ppb - 1)
                !packedI = y * packedWidth + packedX
                !px = packedPixels `VS.unsafeIndex` packedI
                !green = (px `shiftR` 8) .&. 0xFF
                !colorIdx = fromIntegral ((green `shiftR` (subIdx * bpp)) .&. mask) :: Int
             in if colorIdx < paletteLen then palette `VS.unsafeIndex` colorIdx else 0x00000000
  where
    !paletteLen = VS.length palette
