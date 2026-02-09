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

-- Performance: INLINE pragmas and manual loops with unsafeRead/unsafeWrite

-- | Convert quality parameter (0-100) to yac_qi value (0-127)
-- Higher quality → lower qi → finer quantization
qualityToYacQi :: Int -> Int
qualityToYacQi quality =
  let clamped = max 0 (min 100 quality)
      -- Simple linear mapping for now
      -- Quality 100 -> qi 0 (finest)
      -- Quality 0 -> qi 127 (coarsest)
      qi = 127 - (clamped * 127 `div` 100)
   in max 0 (min 127 qi)

-- | Quantize a 4x4 block in place
-- blockType: 0 = Y (AC only), 1 = Y2, 2 = UV, 3 = Y (full)
-- Input: DCT coefficients
-- Output: Quantized coefficients (in place)
{-# INLINE quantizeBlock #-}
quantizeBlock :: DequantFactors -> Int -> VSM.MVector s Int16 -> ST s ()
quantizeBlock factors blockType coeffs = do
  case blockType of
    0 -> quantYAC factors coeffs
    1 -> quantY2 factors coeffs
    2 -> quantUV factors coeffs
    3 -> quantYFull factors coeffs
    _ -> return ()

-- | Quantize Y block (AC only, position 0 is DC from Y2, don't quantize it here)
{-# INLINE quantYAC #-}
quantYAC :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantYAC factors coeffs = do
  let !quant = dqYAC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (quantizeCoeff c quant)
            go (i + 1)
  go 1

-- | Quantize Y2 DC block
{-# INLINE quantY2 #-}
quantY2 :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantY2 factors coeffs = do
  !c0 <- VSM.unsafeRead coeffs 0
  let !dcQuant = dqY2DC factors
  VSM.unsafeWrite coeffs 0 (quantizeCoeff c0 dcQuant)

  let !acQuant = dqY2AC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (quantizeCoeff c acQuant)
            go (i + 1)
  go 1

-- | Quantize UV block
{-# INLINE quantUV #-}
quantUV :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantUV factors coeffs = do
  !c0 <- VSM.unsafeRead coeffs 0
  let !dcQuant = dqUVDC factors
  VSM.unsafeWrite coeffs 0 (quantizeCoeff c0 dcQuant)

  let !acQuant = dqUVAC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (quantizeCoeff c acQuant)
            go (i + 1)
  go 1

-- | Quantize Y block (full, including DC at position 0)
{-# INLINE quantYFull #-}
quantYFull :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantYFull factors coeffs = do
  !c0 <- VSM.unsafeRead coeffs 0
  let !dcQuant = dqYDC factors
  VSM.unsafeWrite coeffs 0 (quantizeCoeff c0 dcQuant)

  let !acQuant = dqYAC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (quantizeCoeff c acQuant)
            go (i + 1)
  go 1

-- | Quantize a single coefficient
-- Formula: quantized = round(coeff / quant) = (abs(coeff) + quant/2) / quant
-- Preserves sign
{-# INLINE quantizeCoeff #-}
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
