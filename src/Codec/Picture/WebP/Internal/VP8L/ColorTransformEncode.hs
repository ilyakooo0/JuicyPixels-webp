{-# LANGUAGE BangPatterns #-}

-- | VP8L color transform encoder.
-- Decorrelates R/B channels from G (and B from R) using per-block
-- linear coefficients found by least-squares regression.
module Codec.Picture.WebP.Internal.VP8L.ColorTransformEncode
  ( ColorTransformResult (..),
    computeColorTransform,
  )
where

import Data.Bits
import qualified Data.Vector.Storable as VS
import Data.Word

-- | Result of computing the color transform
data ColorTransformResult = ColorTransformResult
  { -- | Size bits (2-10), block size = 2^sizeBits
    ctSizeBits :: !Int,
    -- | Transform image: per-block coefficients packed as ARGB pixels
    -- (alpha=255, red=red_to_blue, green=green_to_blue, blue=green_to_red)
    ctTransformImage :: !(VS.Vector Word32),
    -- | Forward-transformed pixels (channel-decorrelated)
    ctTransformedPixels :: !(VS.Vector Word32),
    -- | Width of the transform subresolution image
    ctTransformWidth :: !Int,
    -- | Height of the transform subresolution image
    ctTransformHeight :: !Int
  }
  deriving (Show)

-- | Compute the color transform for an image (or predictor residuals).
-- Finds optimal per-block coefficients and applies the forward transform.
computeColorTransform :: Int -> Int -> Int -> VS.Vector Word32 -> ColorTransformResult
computeColorTransform sizeBits width height pixels =
  let !blockSize = 1 `shiftL` sizeBits
      !transformWidth = (width + blockSize - 1) `shiftR` sizeBits
      !transformHeight = (height + blockSize - 1) `shiftR` sizeBits

      -- Compute optimal coefficients per block via least-squares
      transformImage = VS.generate (transformWidth * transformHeight) $ \i ->
        let !bx = i `mod` transformWidth
            !by = i `div` transformWidth
         in computeBlockCoefficients sizeBits bx by width height pixels

      -- Apply forward transform to all pixels
      transformed = VS.generate (width * height) $ \i ->
        let !x = i `mod` width
            !y = i `div` width
            !pixel = pixels `VS.unsafeIndex` i
            !bx = x `shiftR` sizeBits
            !by = y `shiftR` sizeBits
            !tpx = transformImage `VS.unsafeIndex` (by * transformWidth + bx)
         in applyForwardTransform tpx pixel
   in ColorTransformResult
        { ctSizeBits = sizeBits,
          ctTransformImage = transformImage,
          ctTransformedPixels = transformed,
          ctTransformWidth = transformWidth,
          ctTransformHeight = transformHeight
        }

-- | Compute packed transform pixel for a single block.
-- Uses least-squares to find green_to_red, green_to_blue, red_to_blue.
{-# INLINE computeBlockCoefficients #-}
computeBlockCoefficients :: Int -> Int -> Int -> Int -> Int -> VS.Vector Word32 -> Word32
computeBlockCoefficients sizeBits bx by width height pixels =
  let !blockSize = 1 `shiftL` sizeBits
      !startX = bx * blockSize
      !startY = by * blockSize
      !endX = min (startX + blockSize) width
      !endY = min (startY + blockSize) height

      -- First pass: sum(g*g), sum(g*r), sum(g*b), sum(r*r)
      !(sGG, sGR, sGB, sRR) = collectStats startX startY endX endY width pixels

      -- green_to_red: minimize sum((r - (g2r*g)>>5)^2)
      -- Optimal: g2r = 32 * sum(g*r) / sum(g*g)
      !g2r = if sGG == 0 then 0 else clampCoeff (32 * sGR `quot` sGG)

      -- green_to_blue: minimize sum((b - (g2b*g)>>5)^2)
      !g2b = if sGG == 0 then 0 else clampCoeff (32 * sGB `quot` sGG)

      -- red_to_blue: fit on green-corrected blue
      -- b_adj = b - (g2b*g)>>5, then r2b = 32 * sum(r*b_adj) / sum(r*r)
      !sRBadj = collectRBStats g2b startX startY endX endY width pixels
      !r2b = if sRR == 0 then 0 else clampCoeff (32 * sRBadj `quot` sRR)
   in packTransformPixel g2r g2b r2b

-- | Collect correlation statistics for a block.
-- Returns (sum_g*g, sum_g*r, sum_g*b, sum_r*r).
collectStats :: Int -> Int -> Int -> Int -> Int -> VS.Vector Word32 -> (Int, Int, Int, Int)
collectStats startX startY endX endY width pixels = go 0 0 0 0 startX startY
  where
    go !sGG !sGR !sGB !sRR !x !y
      | y >= endY = (sGG, sGR, sGB, sRR)
      | x >= endX = go sGG sGR sGB sRR startX (y + 1)
      | otherwise =
          let !i = y * width + x
              !px = pixels `VS.unsafeIndex` i
              !g = toSignedChannel (px `shiftR` 8)
              !r = toSignedChannel (px `shiftR` 16)
              !b = toSignedChannel px
           in go (sGG + g * g) (sGR + g * r) (sGB + g * b) (sRR + r * r) (x + 1) y

-- | Collect sum(r * adjusted_blue) for red_to_blue coefficient.
-- adjusted_blue = b - colorTransformDelta(g2b, g)
collectRBStats :: Int -> Int -> Int -> Int -> Int -> Int -> VS.Vector Word32 -> Int
collectRBStats g2b startX startY endX endY width pixels = go 0 startX startY
  where
    go !sRB !x !y
      | y >= endY = sRB
      | x >= endX = go sRB startX (y + 1)
      | otherwise =
          let !i = y * width + x
              !px = pixels `VS.unsafeIndex` i
              !g = toSignedChannel (px `shiftR` 8)
              !r = toSignedChannel (px `shiftR` 16)
              !b = toSignedChannel px
              !bAdj = b - colorTransformDelta g2b g
           in go (sRB + r * bAdj) (x + 1) y

-- | Apply forward color transform to a single pixel.
-- Forward: subtract deltas (inverse adds them back).
{-# INLINE applyForwardTransform #-}
applyForwardTransform :: Word32 -> Word32 -> Word32
applyForwardTransform tpx pixel =
  let -- Unpack coefficients from transform pixel (spec layout)
      !g2r = toSignedChannel tpx -- blue byte = green_to_red
      !g2b = toSignedChannel (tpx `shiftR` 8) -- green byte = green_to_blue
      !r2b = toSignedChannel (tpx `shiftR` 16) -- red byte = red_to_blue

      !a = (pixel `shiftR` 24) .&. 0xFF
      !r = fromIntegral ((pixel `shiftR` 16) .&. 0xFF) :: Int
      !g = fromIntegral ((pixel `shiftR` 8) .&. 0xFF) :: Int
      !b = fromIntegral (pixel .&. 0xFF) :: Int

      -- Signed versions for delta computation
      !gSigned = if g >= 128 then g - 256 else g
      !rSigned = if r >= 128 then r - 256 else r -- forward uses ORIGINAL red

      !newRed = (r - colorTransformDelta g2r gSigned) .&. 0xFF
      !newBlue = (b - colorTransformDelta g2b gSigned - colorTransformDelta r2b rSigned) .&. 0xFF
   in (a `shiftL` 24)
        .|. (fromIntegral newRed `shiftL` 16)
        .|. (fromIntegral g `shiftL` 8)
        .|. fromIntegral newBlue

-- | Pack coefficients into transform pixel.
-- Layout per spec: alpha=255, red=red_to_blue, green=green_to_blue, blue=green_to_red
{-# INLINE packTransformPixel #-}
packTransformPixel :: Int -> Int -> Int -> Word32
packTransformPixel g2r g2b r2b =
  0xFF000000
    .|. (fromIntegral (r2b .&. 0xFF) `shiftL` 16) -- red byte = red_to_blue
    .|. (fromIntegral (g2b .&. 0xFF) `shiftL` 8) -- green byte = green_to_blue
    .|. fromIntegral (g2r .&. 0xFF) -- blue byte = green_to_red

-- | ColorTransformDelta: (t * c) >> 5
{-# INLINE colorTransformDelta #-}
colorTransformDelta :: Int -> Int -> Int
colorTransformDelta t c = (t * c) `shiftR` 5

-- | Extract low 8 bits of a Word32 and convert to signed Int (-128..127)
{-# INLINE toSignedChannel #-}
toSignedChannel :: Word32 -> Int
toSignedChannel w =
  let !v = fromIntegral (w .&. 0xFF) :: Int
   in if v >= 128 then v - 256 else v

-- | Clamp to signed 8-bit range [-128, 127]
{-# INLINE clampCoeff #-}
clampCoeff :: Int -> Int
clampCoeff x
  | x < -128 = -128
  | x > 127 = 127
  | otherwise = x
