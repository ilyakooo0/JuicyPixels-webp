{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.Dequant
  ( DequantFactors (..),
    QuantIndices (..),
    SegmentInfo (..),
    computeDequantFactors,
    dequantizeBlock,
  )
where

import Codec.Picture.WebP.Internal.VP8.Tables
import Control.Monad.ST
import Data.Bits
import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- Performance: INLINE pragmas and manual loops with unsafeRead/unsafeWrite

-- | Quantization indices from frame header
data QuantIndices = QuantIndices
  { qiYacQi :: !Int,
    qiYdcDelta :: !Int,
    qiY2dcDelta :: !Int,
    qiY2acDelta :: !Int,
    qiUvdcDelta :: !Int,
    qiUvacDelta :: !Int
  }
  deriving (Show, Eq)

-- | Segment information
data SegmentInfo = SegmentInfo
  { segmentEnabled :: !Bool,
    segmentUpdateMap :: !Bool,
    segmentAbsoluteMode :: !Bool,
    segmentQuantizer :: !(VU.Vector Int),
    segmentFilterStrength :: !(VU.Vector Int)
  }
  deriving (Show, Eq)

-- | Dequantization factors for a segment
data DequantFactors = DequantFactors
  { dqYDC :: !Int16,
    dqYAC :: !Int16,
    dqY2DC :: !Int16,
    dqY2AC :: !Int16,
    dqUVDC :: !Int16,
    dqUVAC :: !Int16
  }
  deriving (Show, Eq)

-- | Compute dequantization factors for all segments
computeDequantFactors :: QuantIndices -> Maybe SegmentInfo -> V.Vector DequantFactors
computeDequantFactors qi maybeSegInfo =
  let numSegments = case maybeSegInfo of
        Nothing -> 1
        Just info ->
          if segmentEnabled info
            then 4
            else 1
   in V.generate numSegments $ \seg -> computeSegmentDequant qi maybeSegInfo seg

-- | Compute dequantization factors for a specific segment
computeSegmentDequant :: QuantIndices -> Maybe SegmentInfo -> Int -> DequantFactors
computeSegmentDequant qi maybeSegInfo segment =
  let baseQi = qiYacQi qi

      segmentQi = case maybeSegInfo of
        Nothing -> baseQi
        Just info ->
          if segmentEnabled info
            then
              let delta = segmentQuantizer info VU.! segment
               in if segmentAbsoluteMode info
                    then delta
                    else baseQi + delta
            else baseQi

      clampedQi = max 0 (min 127 segmentQi)

      ydc = computeQuantizer clampedQi (qiYdcDelta qi) dcQLookup
      yac = computeQuantizer clampedQi 0 acQLookup
      y2dc = computeQuantizer clampedQi (qiY2dcDelta qi) dcQLookup
      y2ac = computeQuantizer clampedQi (qiY2acDelta qi) acQLookup
      uvdc = computeQuantizer clampedQi (qiUvdcDelta qi) dcQLookup
      uvac = computeQuantizer clampedQi (qiUvacDelta qi) acQLookup

      y2dcAdjusted = y2dc * 2
      y2acAdjusted = max 8 ((y2ac * 155) `div` 100)
      uvdcCapped = min 132 uvdc
   in DequantFactors
        { dqYDC = fromIntegral ydc,
          dqYAC = fromIntegral yac,
          dqY2DC = fromIntegral y2dcAdjusted,
          dqY2AC = fromIntegral y2acAdjusted,
          dqUVDC = fromIntegral uvdcCapped,
          dqUVAC = fromIntegral uvac
        }

-- | Compute a single quantizer value
computeQuantizer :: Int -> Int -> VU.Vector Word16 -> Int
computeQuantizer baseQi delta lookupTable =
  let qi = max 0 (min 127 (baseQi + delta))
   in fromIntegral (lookupTable VU.! qi)

-- | Dequantize a 4x4 block in place
-- blockType: 0 = Y (AC only), 1 = Y2, 2 = UV, 3 = Y (full)
{-# INLINE dequantizeBlock #-}
dequantizeBlock :: DequantFactors -> Int -> VSM.MVector s Int16 -> ST s ()
dequantizeBlock factors blockType coeffs = do
  case blockType of
    0 -> dequantYAC factors coeffs
    1 -> dequantY2 factors coeffs
    2 -> dequantUV factors coeffs
    3 -> dequantYFull factors coeffs
    _ -> return ()

-- | Dequantize Y block (AC only, position 0 already has DC from Y2)
{-# INLINE dequantYAC #-}
dequantYAC :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
dequantYAC factors coeffs = do
  let !factor = dqYAC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (c * factor)
            go (i + 1)
  go 1

-- | Dequantize Y2 DC block
{-# INLINE dequantY2 #-}
dequantY2 :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
dequantY2 factors coeffs = do
  !c0 <- VSM.unsafeRead coeffs 0
  let !dcFactor = dqY2DC factors
  VSM.unsafeWrite coeffs 0 (c0 * dcFactor)
  let !acFactor = dqY2AC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (c * acFactor)
            go (i + 1)
  go 1

-- | Dequantize UV block
{-# INLINE dequantUV #-}
dequantUV :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
dequantUV factors coeffs = do
  !c0 <- VSM.unsafeRead coeffs 0
  let !dcFactor = dqUVDC factors
  VSM.unsafeWrite coeffs 0 (c0 * dcFactor)
  let !acFactor = dqUVAC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (c * acFactor)
            go (i + 1)
  go 1

-- | Dequantize Y block (full, including DC at position 0)
{-# INLINE dequantYFull #-}
dequantYFull :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
dequantYFull factors coeffs = do
  !c0 <- VSM.unsafeRead coeffs 0
  let !dcFactor = dqYDC factors
  VSM.unsafeWrite coeffs 0 (c0 * dcFactor)
  let !acFactor = dqYAC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (c * acFactor)
            go (i + 1)
  go 1
