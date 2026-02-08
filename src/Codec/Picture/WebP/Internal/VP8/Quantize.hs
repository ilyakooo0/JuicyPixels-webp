{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.Quantize
  ( quantizeBlock,
    qualityToYacQi,
  )
where

import Codec.Picture.WebP.Internal.VP8.Dequant
import Control.Monad.ST
import Data.Bits
import Data.Int
import qualified Data.Vector.Storable.Mutable as VSM

-- | Convert quality parameter (0-100) to yac_qi value (0-127)
-- Higher quality → lower qi → finer quantization
-- Uses libwebp-inspired mapping for good perceptual quality
qualityToYacQi :: Int -> Int
qualityToYacQi quality =
  let clamped = max 0 (min 100 quality)
      -- libwebp-inspired mapping:
      -- Quality 0-50: Larger steps for file size reduction
      -- Quality 50-100: Smaller steps for quality preservation
      qi = if clamped <= 50
            then
              -- For low quality, map more aggressively
              -- Quality 0 -> qi 127, Quality 50 -> qi 29
              127 - (clamped * 98 `div` 50)
            else
              -- For high quality, use finer steps
              -- Quality 50 -> qi 29, Quality 100 -> qi 4 (not 0, preserve some quantization)
              29 - ((clamped - 50) * 25 `div` 50)
   in max 4 (min 127 qi)  -- Never go below qi=4 for stability

-- | Quantize a 4x4 block in place
-- blockType: 0 = Y (AC only), 1 = Y2, 2 = UV, 3 = Y (full)
-- Input: DCT coefficients
-- Output: Quantized coefficients (in place)
quantizeBlock :: DequantFactors -> Int -> VSM.MVector s Int16 -> ST s ()
quantizeBlock factors blockType coeffs = do
  case blockType of
    0 -> quantYAC factors coeffs
    1 -> quantY2 factors coeffs
    2 -> quantUV factors coeffs
    3 -> quantYFull factors coeffs
    _ -> return ()

-- | Quantize Y block (AC only, position 0 is DC from Y2, don't quantize it here)
quantYAC :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantYAC factors coeffs = do
  let quant = dqYAC factors
  mapM_
    ( \i -> do
        c <- VSM.read coeffs i
        VSM.write coeffs i (quantizeCoeff c quant)
    )
    [1 .. 15]

-- | Quantize Y2 DC block
quantY2 :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantY2 factors coeffs = do
  c0 <- VSM.read coeffs 0
  let dcQuant = dqY2DC factors
  VSM.write coeffs 0 (quantizeCoeff c0 dcQuant)

  let acQuant = dqY2AC factors
  mapM_
    ( \i -> do
        c <- VSM.read coeffs i
        VSM.write coeffs i (quantizeCoeff c acQuant)
    )
    [1 .. 15]

-- | Quantize UV block
quantUV :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantUV factors coeffs = do
  c0 <- VSM.read coeffs 0
  let dcQuant = dqUVDC factors
  VSM.write coeffs 0 (quantizeCoeff c0 dcQuant)

  let acQuant = dqUVAC factors
  mapM_
    ( \i -> do
        c <- VSM.read coeffs i
        VSM.write coeffs i (quantizeCoeff c acQuant)
    )
    [1 .. 15]

-- | Quantize Y block (full, including DC at position 0)
quantYFull :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantYFull factors coeffs = do
  c0 <- VSM.read coeffs 0
  let dcQuant = dqYDC factors
  VSM.write coeffs 0 (quantizeCoeff c0 dcQuant)

  let acQuant = dqYAC factors
  mapM_
    ( \i -> do
        c <- VSM.read coeffs i
        VSM.write coeffs i (quantizeCoeff c acQuant)
    )
    [1 .. 15]

-- | Quantize a single coefficient
-- Formula: quantized = round(coeff / quant) = (abs(coeff) + quant/2) / quant
-- Preserves sign
quantizeCoeff :: Int16 -> Int16 -> Int16
quantizeCoeff !coeff !quant
  | quant == 0 = 0 -- Avoid division by zero
  | otherwise =
      let absCoeff = abs (fromIntegral coeff :: Int)
          absQuant = abs (fromIntegral quant :: Int)
          -- Add rounding bias (quant / 2)
          quantized = (absCoeff + (absQuant `shiftR` 1)) `div` absQuant
          -- Preserve sign
          result = if coeff < 0 then -quantized else quantized
       in fromIntegral result
