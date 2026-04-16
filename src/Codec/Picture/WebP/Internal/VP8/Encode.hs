{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.Encode
  ( encodeVP8,
    EncodeConfig (..),
    defaultEncodeConfig,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.VP8.BoolEncoder
import Codec.Picture.WebP.Internal.VP8.CoeffStats
import Codec.Picture.WebP.Internal.VP8.ColorConvert (clip255, rgbToYCbCrSharp)
import Codec.Picture.WebP.Internal.VP8.DCT
import Codec.Picture.WebP.Internal.VP8.Dequant
import Codec.Picture.WebP.Internal.VP8.EncodeCoefficients
import Codec.Picture.WebP.Internal.VP8.EncodeHeader
import Codec.Picture.WebP.Internal.VP8.EncodeMode
import Codec.Picture.WebP.Internal.VP8.FilterStrengthSearch (optimizeFilterStrength, optimizeFilterStrengthPerSegment)
import Codec.Picture.WebP.Internal.VP8.IDCT
import Codec.Picture.WebP.Internal.VP8.LoopFilter (applyNormalLoopFilterRow, applyNormalLoopFilterRowSegmented)
import Codec.Picture.WebP.Internal.VP8.ModeSelection
import Codec.Picture.WebP.Internal.VP8.Predict
import Codec.Picture.WebP.Internal.VP8.Quantize (blockOrigVar256, qualityToYacQi, rdModeLambda, ssimTrellisScale, trellisQuantizeBlock)
import Codec.Picture.WebP.Internal.VP8.Tables
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import Data.Int
import Data.List (sort)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- | Encoder configuration
data EncodeConfig = EncodeConfig
  { encQuality :: !Int, -- 0-100 (higher = better quality)
    encFilterLevel :: !Int, -- 0-63 (0 = no loop filter)
    encFilterType :: !Int, -- 0 = normal, 1 = simple
    encUseSegmentation :: !Bool -- Enable segmentation (advanced)
  }
  deriving (Show, Eq)

-- | Default encoder configuration
defaultEncodeConfig :: Int -> EncodeConfig
defaultEncodeConfig quality =
  let qi = qualityToYacQi quality
   in EncodeConfig
        { encQuality = quality,
          -- Filter level derived from qi (not quality) so the non-linear
          -- quality curve flows through: qi/2 maps [0,127] to [0,63].
          encFilterLevel = min 63 $ max 0 $ qi `div` 2,
          encFilterType = 0, -- Normal filter (filters Y+U+V, sub-block edges)
          encUseSegmentation = True -- Enable adaptive QP segmentation
        }

-- | Encode an RGB8 image to VP8 bitstream
-- Returns the raw VP8 data (without WebP container)
--
-- VP8 frame layout (RFC 6386):
--   [uncompressed header (10 bytes)]
--   [partition 0: compressed header + per-MB modes]
--   [DCT partition: per-MB coefficients]
encodeVP8 :: Image PixelRGB8 -> Int -> B.ByteString
encodeVP8 img quality = runST $ do
  -- Step 1: Convert RGB to YCbCr
  (yBuf, uBuf, vBuf) <- rgbToYCbCrSharp img

  let width = imageWidth img
      height = imageHeight img
      paddedW = ((width + 15) `div` 16) * 16
      paddedH = ((height + 15) `div` 16) * 16
      mbCols = paddedW `div` 16
      mbRows = paddedH `div` 16

  -- Step 2: Set up encoder configuration
  let config = defaultEncodeConfig quality
      qi = qualityToYacQi quality
      -- Negative UV delta gives chroma finer quantization than luma.
      -- Human vision is more sensitive to chroma shifts than luma detail,
      -- so this improves perceived quality at minimal bitrate cost.
      !uvDelta = negate (max 0 (min 15 (qi `div` 8)))
      quantIndices =
        QuantIndices
          { qiYacQi = qi,
            qiYdcDelta = 0,
            qiY2dcDelta = 0,
            qiY2acDelta = 0,
            qiUvdcDelta = uvDelta,
            qiUvacDelta = uvDelta
          }

  -- Step 3: Compute per-MB spatial activity (needed for both SNS and activity masking)
  alphas <- computeMBAlphas yBuf paddedW mbRows mbCols
  let !activityWeights = computeActivityWeights alphas (mbRows * mbCols)

  -- Step 3b: Spatial Noise Shaping (SNS) segmentation
  (mSegHeaderInfo, dequantFactorsVec, segLambdas, mSegEncInfo) <-
    if encUseSegmentation config && qi >= 8 && mbRows * mbCols >= 4
      then do
        let (!segMap, !segDeltas, !c0, !c1, !c2, !c3) = classifySegmentsSNS alphas qi
        if not (VU.any (/= 0) segDeltas)
          then do
            let dqVec = computeDequantFactors quantIndices Nothing
                lams = VU.singleton (rdModeLambda (dqVec V.! 0))
            return (Nothing, dqVec, lams, Nothing)
          else do
            let (sp0, sp1, sp2) = computeSegmentProbs c0 c1 c2 c3
                segFilterDeltas = computeSegmentFilterDeltas qi segDeltas
                segInfo =
                  SegmentInfo
                    { segmentEnabled = True,
                      segmentUpdateMap = True,
                      segmentAbsoluteMode = False,
                      segmentQuantizer = segDeltas,
                      segmentFilterStrength = segFilterDeltas,
                      segmentTreeProbs = (sp0, sp1, sp2)
                    }
                dqVec = computeDequantFactors quantIndices (Just segInfo)
                lams = VU.generate 4 $ \s -> rdModeLambda (dqVec V.! s)
            return (Just (segInfo, sp0, sp1, sp2), dqVec, lams, Just (segMap, sp0, sp1, sp2, segFilterDeltas))
      else do
        let dqVec = computeDequantFactors quantIndices Nothing
            lams = VU.singleton (rdModeLambda (dqVec V.! 0))
        return (Nothing, dqVec, lams, Nothing)

  -- Step 4: Allocate reconstruction buffers (for prediction)
  let !ySize = paddedW * paddedH
      !uvSize = (paddedW `div` 2) * (paddedH `div` 2)
  yRecon <- VSM.replicate ySize 128
  uRecon <- VSM.replicate uvSize 128
  vRecon <- VSM.replicate uvSize 128

  -- Step 5: Pass 1 — encode with default probs, collect coefficient statistics
  --         Also save pre-filter reconstruction for filter strength search
  --         Skip mode disabled (Nothing) — we just count skip MBs for prob computation
  stats <- newCoeffStats
  let noUpdateFlags = VU.replicate 1056 False
      defaultFilterLevel = encFilterLevel config
      compressedHeaderEnc1 = generateCompressedHeader quantIndices defaultFilterLevel (encFilterType config) mSegHeaderInfo defaultCoeffProbs noUpdateFlags Nothing

  -- Allocate pre-filter buffers (capture reconstruction before loop filter)
  yPreFilter <- VSM.new ySize
  uPreFilter <- VSM.new uvSize
  vPreFilter <- VSM.new uvSize

  (modeEnc1, coeffEnc1, skipCount1) <-
    encodeMacroblocks
      yBuf uBuf vBuf yRecon uRecon vRecon
      paddedW paddedH mbRows mbCols
      dequantFactorsVec segLambdas mSegEncInfo defaultCoeffProbs
      compressedHeaderEnc1 initBoolEncoder
      defaultFilterLevel (Just stats)
      (Just (yPreFilter, uPreFilter, vPreFilter))
      Nothing
      activityWeights

  -- Step 6: Adaptive filter strength search (per-segment when segmentation active)
  (optFilterLevel, mSegHeaderInfo2, mSegEncInfo2) <-
    if defaultFilterLevel > 0
      then case mSegEncInfo of
        Just (segMap, sp0, sp1, sp2, qpFilterDeltas) -> do
          (baseLevel, optDeltas) <-
            optimizeFilterStrengthPerSegment
              yBuf uBuf vBuf yPreFilter uPreFilter vPreFilter
              paddedW mbRows mbCols defaultFilterLevel qpFilterDeltas segMap
          let mSHI2 = case mSegHeaderInfo of
                Just (si, p0, p1, p2) -> Just (si {segmentFilterStrength = optDeltas}, p0, p1, p2)
                Nothing -> Nothing
              mSEI2 = Just (segMap, sp0, sp1, sp2, optDeltas)
          return (baseLevel, mSHI2, mSEI2)
        Nothing -> do
          level <-
            optimizeFilterStrength
              yBuf uBuf vBuf yPreFilter uPreFilter vPreFilter
              paddedW mbRows mbCols defaultFilterLevel Nothing
          return (level, mSegHeaderInfo, mSegEncInfo)
      else return (0, mSegHeaderInfo, mSegEncInfo)

  -- Step 7: Compute optimal coefficient probabilities from statistics
  optimalProbs <- computeOptimalProbs stats
  (updatedProbs, updateFlags) <- decideUpdates stats optimalProbs
  let hasUpdates = VU.any id updateFlags

  -- Step 7b: Compute skip probability from pass 1 statistics
  let !totalMBs = mbRows * mbCols
      !nonSkipMBs = totalMBs - skipCount1
      !hasSkipMBs = skipCount1 > 0
      -- prob_skip_false = probability that a MB is NOT skipped (has coefficients)
      !probSkipFalse = fromIntegral (max 1 (min 255 ((256 * nonSkipMBs + totalMBs `div` 2) `div` max 1 totalMBs))) :: Word8
      !mSkipProb = if hasSkipMBs then Just probSkipFalse else Nothing

      filterDeltasChanged = case (mSegEncInfo, mSegEncInfo2) of
        (Just (_, _, _, _, oldD), Just (_, _, _, _, newD)) -> oldD /= newD
        _ -> False
      needsReencode = hasUpdates || optFilterLevel /= defaultFilterLevel || filterDeltasChanged || hasSkipMBs

  if not needsReencode
    then do
      -- No changes needed: use pass 1 output as-is
      let partition0 = finalizeBoolEncoder modeEnc1
          dctPartition = finalizeBoolEncoder coeffEnc1
          uncompHeader = generateUncompressedHeader width height (B.length partition0)
      return $ uncompHeader <> partition0 <> dctPartition
    else do
      -- Step 8: Reset reconstruction buffers for pass 2
      VSM.set yRecon 128
      VSM.set uRecon 128
      VSM.set vRecon 128

      -- Step 9: Pass 2 — re-encode with optimal filter level, probs, skip mode,
      --         and optimized per-segment filter deltas.
      --         Also collect statistics: mode decisions with updated probs may
      --         shift the coefficient distribution, so a third pass can help.
      let probs2 = if hasUpdates then updatedProbs else defaultCoeffProbs
          flags2 = if hasUpdates then updateFlags else noUpdateFlags
          compressedHeaderEnc2 = generateCompressedHeader quantIndices optFilterLevel (encFilterType config) mSegHeaderInfo2 probs2 flags2 mSkipProb

      stats2 <- newCoeffStats
      (modeEnc2, coeffEnc2, skipCount2) <-
        encodeMacroblocks
          yBuf uBuf vBuf yRecon uRecon vRecon
          paddedW paddedH mbRows mbCols
          dequantFactorsVec segLambdas mSegEncInfo2 probs2
          compressedHeaderEnc2 initBoolEncoder
          optFilterLevel (Just stats2)
          Nothing
          mSkipProb
          activityWeights

      -- Step 10: Check if pass 3 would improve probabilities.
      -- Recompute optimal probabilities from pass 2's actual coefficient
      -- distribution and re-encode if they differ from what pass 2 used.
      optimalProbs2 <- computeOptimalProbs stats2
      (updatedProbs2, updateFlags2) <- decideUpdates stats2 optimalProbs2
      let !hasUpdates2 = VU.any id updateFlags2
          !probs3 = if hasUpdates2 then updatedProbs2 else defaultCoeffProbs
          !flags3 = if hasUpdates2 then updateFlags2 else noUpdateFlags
          -- Updated skip probability from pass 2 statistics
          !nonSkipMBs2 = totalMBs - skipCount2
          !probSkipFalse2 = fromIntegral (max 1 (min 255 ((256 * nonSkipMBs2 + totalMBs `div` 2) `div` max 1 totalMBs))) :: Word8
          !mSkipProb2 = if skipCount2 > 0 then Just probSkipFalse2 else Nothing
          !needsPass3 = probs3 /= probs2 || mSkipProb2 /= mSkipProb

      if not needsPass3
        then do
          -- Pass 2 probabilities already converged — use pass 2 output
          let partition0 = finalizeBoolEncoder modeEnc2
              dctPartition = finalizeBoolEncoder coeffEnc2
              uncompHeader = generateUncompressedHeader width height (B.length partition0)
          return $ uncompHeader <> partition0 <> dctPartition
        else do
          -- Step 11: Pass 3 — re-encode with converged probabilities and skip prob
          VSM.set yRecon 128
          VSM.set uRecon 128
          VSM.set vRecon 128

          let compressedHeaderEnc3 = generateCompressedHeader quantIndices optFilterLevel (encFilterType config) mSegHeaderInfo2 probs3 flags3 mSkipProb2

          (modeEnc3, coeffEnc3, _) <-
            encodeMacroblocks
              yBuf uBuf vBuf yRecon uRecon vRecon
              paddedW paddedH mbRows mbCols
              dequantFactorsVec segLambdas mSegEncInfo2 probs3
              compressedHeaderEnc3 initBoolEncoder
              optFilterLevel Nothing
              Nothing
              mSkipProb2
              activityWeights

          let partition0 = finalizeBoolEncoder modeEnc3
              dctPartition = finalizeBoolEncoder coeffEnc3
              uncompHeader = generateUncompressedHeader width height (B.length partition0)
          return $ uncompHeader <> partition0 <> dctPartition

-- | Encode all macroblocks, writing modes to modeEnc and coefficients to coeffEnc
encodeMacroblocks ::
  VSM.MVector s Word8 -> -- Y original
  VSM.MVector s Word8 -> -- U original
  VSM.MVector s Word8 -> -- V original
  VSM.MVector s Word8 -> -- Y reconstruction
  VSM.MVector s Word8 -> -- U reconstruction
  VSM.MVector s Word8 -> -- V reconstruction
  Int ->
  Int -> -- Padded width, height
  Int ->
  Int -> -- MB rows, cols
  V.Vector DequantFactors -> -- Per-segment dequant factors (length 1 or 4)
  VU.Vector Int -> -- Per-segment RDO lambdas
  Maybe (VU.Vector Word8, Word8, Word8, Word8, VU.Vector Int) -> -- Segment map + 3 tree probs + filter deltas (Nothing = no segments)
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder -> -- Mode encoder (partition 0)
  BoolEncoder -> -- Coefficient encoder (DCT partition)
  Int -> -- Filter level for per-row loop filter
  Maybe (CoeffStats s) -> -- Optional coefficient statistics accumulator
  Maybe (VSM.MVector s Word8, VSM.MVector s Word8, VSM.MVector s Word8) -> -- Pre-filter buffers (save recon before loop filter)
  Maybe Word8 -> -- Skip mode: Just probSkipFalse to enable, Nothing to disable
  VU.Vector Int -> -- Per-MB activity weights (8.8 fixed point, 256 = 1.0)
  ST s (BoolEncoder, BoolEncoder, Int)
  -- Returns: (modeEncoder, coeffEncoder, skipCount)
encodeMacroblocks yOrig uOrig vOrig yRecon uRecon vRecon paddedW paddedH mbRows mbCols dqVec segLambdas mSegEncInfo coeffProbs modeEnc coeffEnc filterLevel mStats mPreFilterBufs mSkipProb actWeights = do
  -- Allocate above NZ tracking arrays (persist across MB rows)
  aboveNzY <- VSM.replicate (mbCols * 4) (0 :: Word8) -- 4 Y columns per MB
  aboveNzU <- VSM.replicate (mbCols * 2) (0 :: Word8) -- 2 U columns per MB
  aboveNzV <- VSM.replicate (mbCols * 2) (0 :: Word8) -- 2 V columns per MB
  aboveNzDC <- VSM.replicate mbCols (0 :: Word8) -- 1 DC per MB
  -- B_PRED sub-block mode context (4 modes per MB column for bottom row)
  -- Non-B_PRED MBs store 0 (B_DC_PRED) as default context
  aboveBModes <- VSM.replicate (mbCols * 4) (0 :: Word8)
  let loop !mbY !mbX !mEnc !cEnc !skipCount !leftNzY0 !leftNzY1 !leftNzY2 !leftNzY3 !leftNzU0 !leftNzU1 !leftNzV0 !leftNzV1 !leftNzDC !leftBM0 !leftBM1 !leftBM2 !leftBM3
        | mbY >= mbRows = return (mEnc, cEnc, skipCount)
        | mbX >= mbCols = do
            -- Save pre-filter reconstruction before loop filter modifies it
            case mPreFilterBufs of
              Just (yPF, uPF, vPF) -> do
                let !yRowStart = mbY * 16 * paddedW
                    !yRowLen = 16 * paddedW
                    !uvStride = paddedW `div` 2
                    !uvRowStart = mbY * 8 * uvStride
                    !uvRowLen = 8 * uvStride
                VSM.copy (VSM.slice yRowStart yRowLen yPF) (VSM.slice yRowStart yRowLen yRecon)
                VSM.copy (VSM.slice uvRowStart uvRowLen uPF) (VSM.slice uvRowStart uvRowLen uRecon)
                VSM.copy (VSM.slice uvRowStart uvRowLen vPF) (VSM.slice uvRowStart uvRowLen vRecon)
              Nothing -> return ()
            -- Apply per-row loop filter to completed row
            when (filterLevel > 0) $
              case mSegEncInfo of
                Just (segMap, _, _, _, segFD) ->
                  applyNormalLoopFilterRowSegmented yRecon paddedW uRecon (paddedW `div` 2) vRecon (paddedW `div` 2) mbY mbCols filterLevel segFD segMap
                Nothing ->
                  applyNormalLoopFilterRow yRecon paddedW uRecon (paddedW `div` 2) vRecon (paddedW `div` 2) mbY mbCols filterLevel
            -- New row: reset left NZ and left B modes to 0
            loop (mbY + 1) 0 mEnc cEnc skipCount 0 0 0 0 0 0 0 0 0 0 0 0 0
        | otherwise = do
            -- Segment handling: look up per-MB segment, write ID, select per-segment params
            let !mbIdx = mbY * mbCols + mbX
                (!segDq, !segLam, !mEncSeg) = case mSegEncInfo of
                  Nothing ->
                    (dqVec V.! 0, segLambdas VU.! 0, mEnc)
                  Just (segMap, sp0, sp1, sp2, _) ->
                    let !s = fromIntegral (segMap VU.! mbIdx)
                     in (dqVec V.! s, segLambdas VU.! s, encodeSegmentId s sp0 sp1 sp2 mEnc)
                -- Per-MB activity masking: scale lambda by spatial activity weight
                !actW = actWeights VU.! mbIdx
                !adjLam = max 1 ((segLam * actW) `div` 256)

            (mEnc', cEnc', isSkip, lY0, lY1, lY2, lY3, lU0, lU1, lV0, lV1, lDC, lBM0, lBM1, lBM2, lBM3) <-
              encodeMacroblock
                yOrig
                uOrig
                vOrig
                yRecon
                uRecon
                vRecon
                paddedW
                paddedH
                mbY
                mbX
                segDq
                adjLam
                actW
                coeffProbs
                mEncSeg
                cEnc
                aboveNzY
                aboveNzU
                aboveNzV
                aboveNzDC
                aboveBModes
                leftNzY0
                leftNzY1
                leftNzY2
                leftNzY3
                leftNzU0
                leftNzU1
                leftNzV0
                leftNzV1
                leftNzDC
                leftBM0
                leftBM1
                leftBM2
                leftBM3
                mStats
                mSkipProb
            let !skipCount' = if isSkip then skipCount + 1 else skipCount
            loop mbY (mbX + 1) mEnc' cEnc' skipCount' lY0 lY1 lY2 lY3 lU0 lU1 lV0 lV1 lDC lBM0 lBM1 lBM2 lBM3

  loop 0 0 modeEnc coeffEnc 0 0 0 0 0 0 0 0 0 0 0 0 0 0

-- | Encode a single macroblock
-- Modes go to modeEnc (partition 0), coefficients go to coeffEnc (DCT partition)
-- Returns updated encoders, NZ state, and B mode context for left neighbor
encodeMacroblock ::
  VSM.MVector s Word8 -> -- Y original
  VSM.MVector s Word8 -> -- U original
  VSM.MVector s Word8 -> -- V original
  VSM.MVector s Word8 -> -- Y reconstruction
  VSM.MVector s Word8 -> -- U reconstruction
  VSM.MVector s Word8 -> -- V reconstruction
  Int ->
  Int -> -- Padded width, height
  Int ->
  Int -> -- MB row, col
  DequantFactors ->
  Int -> -- RDO lambda (activity-adjusted)
  Int -> -- Activity scale (8.8 fixed, 256 = unity) — for trellis lambda masking
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  BoolEncoder -> -- Mode and coefficient encoders
  VSM.MVector s Word8 -> -- aboveNzY (mbCols * 4)
  VSM.MVector s Word8 -> -- aboveNzU (mbCols * 2)
  VSM.MVector s Word8 -> -- aboveNzV (mbCols * 2)
  VSM.MVector s Word8 -> -- aboveNzDC (mbCols)
  VSM.MVector s Word8 -> -- aboveBModes (mbCols * 4)
  Int ->
  Int ->
  Int ->
  Int -> -- leftNzY[0..3]
  Int ->
  Int -> -- leftNzU[0..1]
  Int ->
  Int -> -- leftNzV[0..1]
  Int -> -- leftNzDC
  Int ->
  Int ->
  Int ->
  Int -> -- leftBMode[0..3]
  Maybe (CoeffStats s) -> -- Optional coefficient statistics
  Maybe Word8 -> -- Skip mode: Just probSkipFalse to enable, Nothing to disable
  ST s (BoolEncoder, BoolEncoder, Bool, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
  -- Returns: (mEnc, cEnc, isSkip, leftNzY0..3, leftNzU0..1, leftNzV0..1, leftNzDC, leftBM0..3)
encodeMacroblock yOrig uOrig vOrig yRecon uRecon vRecon paddedW _paddedH mbY mbX dequantFactors lambda actScale coeffProbs mEnc cEnc aboveNzY aboveNzU aboveNzV aboveNzDC aboveBModes leftNzY0 leftNzY1 leftNzY2 leftNzY3 leftNzU0 leftNzU1 leftNzV0 leftNzV1 leftNzDC leftBM0 leftBM1 leftBM2 leftBM3 mStats mSkipProb = do
  let mbXpix = mbX * 16
      mbYpix = mbY * 16

  -- Read above B-modes early (needed for B_PRED RDO and encoding)
  aBM0 <- VSM.read aboveBModes (mbX * 4)
  aBM1 <- VSM.read aboveBModes (mbX * 4 + 1)
  aBM2 <- VSM.read aboveBModes (mbX * 4 + 2)
  aBM3 <- VSM.read aboveBModes (mbX * 4 + 3)

  -- Read above NZ context early (needed for RDO mode selection)
  aNzY0 <- fromIntegral <$> VSM.read aboveNzY (mbX * 4)
  aNzY1 <- fromIntegral <$> VSM.read aboveNzY (mbX * 4 + 1)
  aNzY2 <- fromIntegral <$> VSM.read aboveNzY (mbX * 4 + 2)
  aNzY3 <- fromIntegral <$> VSM.read aboveNzY (mbX * 4 + 3)
  aNzDC <- fromIntegral <$> VSM.read aboveNzDC mbX

  -- Step 1: Select best i16 Y mode using RDO
  (i16Mode, i16Cost) <- selectIntra16x16ModeRDO yOrig yRecon paddedW mbXpix mbYpix dequantFactors lambda actScale coeffProbs aNzY0 aNzY1 aNzY2 aNzY3 leftNzY0 leftNzY1 leftNzY2 leftNzY3 aNzDC leftNzDC

  -- Step 2: Select best B_PRED modes using RDO (modifies yRecon's MB area)
  (bpredModes, bpredCost) <- selectBPredModeRDO yOrig yRecon paddedW mbXpix mbYpix dequantFactors lambda actScale coeffProbs (fromIntegral aBM0) (fromIntegral aBM1) (fromIntegral aBM2) (fromIntegral aBM3) leftBM0 leftBM1 leftBM2 leftBM3 aNzY0 aNzY1 aNzY2 aNzY3 leftNzY0 leftNzY1 leftNzY2 leftNzY3
  -- yRecon now has B_PRED reconstruction; if i16 wins, encodeYBlocks will overwrite it

  -- True RDO: mode encoding costs already included in i16Cost and bpredCost
  let useBPred = bpredCost < i16Cost

  -- Step 3: Select best UV mode using RDO (both U and V)
  let chromaX = mbX * 8
      chromaY = mbY * 8
  (uvPredMode, _) <- selectChromaModeRDO uOrig uRecon vOrig vRecon (paddedW `div` 2) chromaX chromaY dequantFactors lambda actScale coeffProbs

  if useBPred
    then do
      -- === B_PRED path ===
      -- Phase 1: Encode coefficients to determine skip status
      -- (Side effects on yRecon, aboveNz arrays are always needed)
      (cEnc1, anyYNz, newLeftY0, newLeftY1, newLeftY2, newLeftY3) <-
        encodeYBlocksBPred
          yOrig
          yRecon
          paddedW
          mbXpix
          mbYpix
          bpredModes
          dequantFactors
          lambda
          actScale
          coeffProbs
          cEnc
          aboveNzY
          mbX
          leftNzY0
          leftNzY1
          leftNzY2
          leftNzY3
          mStats

      -- B_PRED: no Y2, so DC NZ = 0
      VSM.write aboveNzDC mbX 0

      -- Update above B modes with bottom row (blocks 12-15)
      VSM.write aboveBModes (mbX * 4) (bpredModes VS.! 12)
      VSM.write aboveBModes (mbX * 4 + 1) (bpredModes VS.! 13)
      VSM.write aboveBModes (mbX * 4 + 2) (bpredModes VS.! 14)
      VSM.write aboveBModes (mbX * 4 + 3) (bpredModes VS.! 15)

      -- Encode chroma
      (cEnc2, anyUNz, newLeftU0, newLeftU1) <-
        encodeChromaBlocks
          uOrig
          uRecon
          (paddedW `div` 2)
          chromaX
          chromaY
          uvPredMode
          dequantFactors
          lambda
          actScale
          coeffProbs
          cEnc1
          2
          aboveNzU
          mbX
          leftNzU0
          leftNzU1
          mStats

      (cEnc3, anyVNz, newLeftV0, newLeftV1) <-
        encodeChromaBlocks
          vOrig
          vRecon
          (paddedW `div` 2)
          chromaX
          chromaY
          uvPredMode
          dequantFactors
          lambda
          actScale
          coeffProbs
          cEnc2
          2
          aboveNzV
          mbX
          leftNzV0
          leftNzV1
          mStats

      -- B_PRED: decoder has no skip support for B_PRED MBs, so always encode coefficients.
      -- Write B_PRED Y mode + sub-block modes + UV mode to partition 0 (no skip flag).
      let mEnc1' = encodeYModeBPred mEnc
          mEnc2' = encodeBPredModesToStream bpredModes aBM0 aBM1 aBM2 aBM3 leftBM0 leftBM1 leftBM2 leftBM3 mEnc1'
          mEnc3' = encodeUVMode uvPredMode mEnc2'

      -- Right column B modes (blocks 3,7,11,15) for next MB's left context
      let !newLBM0 = fromIntegral (bpredModes VS.! 3)
          !newLBM1 = fromIntegral (bpredModes VS.! 7)
          !newLBM2 = fromIntegral (bpredModes VS.! 11)
          !newLBM3 = fromIntegral (bpredModes VS.! 15)

      return (mEnc3', cEnc3, False, newLeftY0, newLeftY1, newLeftY2, newLeftY3, newLeftU0, newLeftU1, newLeftV0, newLeftV1, 0, newLBM0, newLBM1, newLBM2, newLBM3)
    else do
      -- === i16 path ===
      -- Phase 1: Encode coefficients to determine skip status
      aNzDC <- VSM.read aboveNzDC mbX
      (cEnc1, y2nz, anyYNz, newLeftY0, newLeftY1, newLeftY2, newLeftY3) <-
        encodeYBlocks
          yOrig
          yRecon
          paddedW
          mbXpix
          mbYpix
          i16Mode
          dequantFactors
          lambda
          actScale
          coeffProbs
          cEnc
          aboveNzY
          mbX
          leftNzY0
          leftNzY1
          leftNzY2
          leftNzY3
          (fromIntegral aNzDC)
          leftNzDC
          mStats

      VSM.write aboveNzDC mbX (if y2nz then 1 else 0)

      -- Non-B_PRED: above B modes default to 0 (B_DC_PRED)
      VSM.write aboveBModes (mbX * 4) 0
      VSM.write aboveBModes (mbX * 4 + 1) 0
      VSM.write aboveBModes (mbX * 4 + 2) 0
      VSM.write aboveBModes (mbX * 4 + 3) 0

      (cEnc2, anyUNz, newLeftU0, newLeftU1) <-
        encodeChromaBlocks
          uOrig
          uRecon
          (paddedW `div` 2)
          chromaX
          chromaY
          uvPredMode
          dequantFactors
          lambda
          actScale
          coeffProbs
          cEnc1
          2
          aboveNzU
          mbX
          leftNzU0
          leftNzU1
          mStats

      (cEnc3, anyVNz, newLeftV0, newLeftV1) <-
        encodeChromaBlocks
          vOrig
          vRecon
          (paddedW `div` 2)
          chromaX
          chromaY
          uvPredMode
          dequantFactors
          lambda
          actScale
          coeffProbs
          cEnc2
          2
          aboveNzV
          mbX
          leftNzV0
          leftNzV1
          mStats

      -- Phase 2: Determine skip status and write to partition 0
      let !isSkip = not (anyYNz || anyUNz || anyVNz)

      -- Write i16 mode + UV mode FIRST, then skip flag
      -- (decoder reads: y_mode, uv_mode, skip_flag — in that order)
      let mEnc1' = encodeYMode i16Mode mEnc
          mEnc2' = encodeUVMode uvPredMode mEnc1'
          !mEnc3' = case mSkipProb of
            Just prob -> boolWrite prob isSkip mEnc2'
            Nothing -> mEnc2'

      -- Phase 3: Discard coefficient data for skip MBs
      let !finalCEnc = case mSkipProb of
            Just _ | isSkip -> cEnc
            _ -> cEnc3

      let !newLeftDC = if y2nz then 1 else 0
      return (mEnc3', finalCEnc, isSkip, newLeftY0, newLeftY1, newLeftY2, newLeftY3, newLeftU0, newLeftU1, newLeftV0, newLeftV1, newLeftDC, 0, 0, 0, 0)

-- | Encode 16 B_PRED sub-block modes to bitstream with above/left context.
-- Pure function: walks the kfBmodeTree for each sub-block.
encodeBPredModesToStream ::
  VS.Vector Word8 -> -- 16 sub-block modes
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 -> -- aboveBModes (from above MB's bottom row)
  Int ->
  Int ->
  Int ->
  Int -> -- leftBModes (from left MB's right column)
  BoolEncoder ->
  BoolEncoder
encodeBPredModesToStream modes aBM0 aBM1 aBM2 aBM3 lBM0 lBM1 lBM2 lBM3 enc =
  let go !bi !e
        | bi >= 16 = e
        | otherwise =
            let !row = bi `shiftR` 2
                !col = bi .&. 3
                !above =
                  if row == 0
                    then fromIntegral $ case col of 0 -> aBM0; 1 -> aBM1; 2 -> aBM2; _ -> aBM3
                    else fromIntegral $ modes VS.! ((row - 1) * 4 + col)
                !left =
                  if col == 0
                    then case row of 0 -> lBM0; 1 -> lBM1; 2 -> lBM2; _ -> lBM3
                    else fromIntegral $ modes VS.! (row * 4 + col - 1)
                !mode = fromIntegral (modes VS.! bi)
             in go (bi + 1) (encodeBSubMode above left mode e)
   in go 0 enc

-- | Encode Y blocks for a macroblock (16x16) with NZ context tracking
-- Processes: Y2 DC block first, then 16 Y AC blocks in raster order
-- Updates aboveNzY with bottom row NZ, returns right column NZ
encodeYBlocks ::
  VSM.MVector s Word8 -> -- Y original
  VSM.MVector s Word8 -> -- Y reconstruction (will contain prediction)
  Int -> -- Stride
  Int ->
  Int -> -- X, Y position
  Int -> -- Prediction mode (0-3)
  DequantFactors ->
  Int -> -- RDO lambda for trellis quantization
  Int -> -- Activity scale (8.8 fixed, 256 = unity) for trellis lambda masking
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  VSM.MVector s Word8 -> -- aboveNzY (mbCols*4, read top row, write bottom row)
  Int -> -- mbX
  Int ->
  Int ->
  Int ->
  Int -> -- leftNzY[0..3]
  Int ->
  Int -> -- aboveDcNz, leftDcNz
  Maybe (CoeffStats s) -> -- Optional coefficient statistics
  ST s (BoolEncoder, Bool, Bool, Int, Int, Int, Int)
  -- Returns: (encoder, y2nz, anyYNz, leftNzY0..3)
encodeYBlocks yOrig yRecon stride x y predMode dequantFactors lambda actScale coeffProbs enc aboveNzY mbX leftNzY0 leftNzY1 leftNzY2 leftNzY3 aboveDcNz leftDcNz mStats = do
  -- Create temporary buffer for prediction (don't overwrite reconstruction yet)
  predBuf <- VSM.clone yRecon

  -- Apply prediction to temporary buffer
  predict16x16 predMode predBuf stride x y

  -- Collect 16 Y block DCs for Y2 by doing forward DCT on all blocks
  y2DCs <- VSM.new 16
  residualBlocks <- VSM.new (16 * 16) -- Store all 16 blocks for later encoding

  -- First pass: Compute all residuals and DCTs, collect DCs
  forM_ [0 .. 15] $ \blockIdx -> do
    let subX = (blockIdx `mod` 4) * 4
        subY = (blockIdx `div` 4) * 4

    -- Allocate temporary residual block
    residuals <- VSM.new 16

    -- Compute residuals (original - prediction)
    forM_ [0 .. 3] $ \row ->
      forM_ [0 .. 3] $ \col -> do
        let px = x + subX + col
            py = y + subY + row
            idx = py * stride + px
        orig <- VSM.read yOrig idx
        pred <- VSM.read predBuf idx
        let residual = fromIntegral orig - fromIntegral pred :: Int16
        VSM.write residuals (row * 4 + col) residual

    -- Forward DCT
    fdct4x4 residuals

    -- Extract DC for Y2
    dc <- VSM.read residuals 0
    VSM.write y2DCs blockIdx dc

    -- Copy residuals to storage for later encoding
    forM_ [0 .. 15] $ \i -> do
      r <- VSM.read residuals i
      VSM.write residualBlocks (blockIdx * 16 + i) r

  -- Forward WHT on Y2 DCs
  fwht4x4 y2DCs

  -- Quantize Y2 (trellis-optimized)
  let !dcCtx = min 2 (aboveDcNz + leftDcNz)
  _ <- trellisQuantizeBlock dequantFactors 1 y2DCs coeffProbs dcCtx 0 256 actScale

  -- ENCODE Y2 FIRST
  -- blockType=1 for Y2 (i16-DC per libwebp convention)
  (enc1, y2nz) <- encodeCoefficients y2DCs coeffProbs 1 dcCtx 0 enc
  case mStats of
    Just s -> countCoefficients y2DCs 1 dcCtx 0 s
    Nothing -> return ()

  -- Dequantize Y2 for reconstruction
  dequantizeBlock dequantFactors 1 y2DCs

  -- Inverse WHT to get reconstructed DC values for each block
  reconY2DCs <- iwht4x4 y2DCs

  -- Read above NZ for this MB's Y columns (from previous MB row's bottom)
  aNzCol0 <- VSM.read aboveNzY (mbX * 4)
  aNzCol1 <- VSM.read aboveNzY (mbX * 4 + 1)
  aNzCol2 <- VSM.read aboveNzY (mbX * 4 + 2)
  aNzCol3 <- VSM.read aboveNzY (mbX * 4 + 3)

  -- NZ tracking grid for 16 sub-blocks (0 or 1 each)
  nzGrid <- VSM.replicate 16 (0 :: Word8)

  -- Encode 16 Y AC blocks in raster order with NZ context tracking
  -- For block at grid (row, col):
  --   above_nz: row==0 → aboveNzY[col], else → nzGrid[(row-1)*4+col]
  --   left_nz: col==0 → leftNzY[row], else → nzGrid[row*4+col-1]
  --   ctx = min 2 (above_nz + left_nz)
  let encodeYBlock !blockIdx !e !anyAcNz
        | blockIdx >= 16 = return (e, anyAcNz)
        | otherwise = do
            let !row = blockIdx `div` 4
                !col = blockIdx `mod` 4

            -- Get above NZ
            aboveNz <-
              if row == 0
                then return $ fromIntegral $ case col of
                  0 -> aNzCol0
                  1 -> aNzCol1
                  2 -> aNzCol2
                  _ -> aNzCol3
                else fromIntegral <$> VSM.read nzGrid ((row - 1) * 4 + col)

            -- Get left NZ
            leftNz <-
              if col == 0
                then return $ case row of
                  0 -> leftNzY0
                  1 -> leftNzY1
                  2 -> leftNzY2
                  _ -> leftNzY3
                else fromIntegral <$> VSM.read nzGrid (row * 4 + col - 1)

            -- Get stored residuals for this block
            residuals <- VSM.new 16
            forM_ [0 .. 15] $ \i -> do
              r <- VSM.read residualBlocks (blockIdx * 16 + i)
              VSM.write residuals i r

            -- DC was already extracted for Y2, clear it
            VSM.write residuals 0 0

            -- Trellis-quantize AC coefficients (SSIM-weighted distortion)
            let !ctx = min 2 (aboveNz + leftNz)
            !yVar256 <- blockOrigVar256 yOrig stride (x + col * 4) (y + row * 4)
            let !ySsScale = ssimTrellisScale yVar256
            _ <- trellisQuantizeBlock dequantFactors 0 residuals coeffProbs ctx 1 ySsScale actScale

            -- Save quantized values for reconstruction (avoid double-quantization)
            forM_ [0 .. 15] $ \i -> do
              q <- VSM.unsafeRead residuals i
              VSM.unsafeWrite residualBlocks (blockIdx * 16 + i) q

            -- Encode AC coefficients with NZ context
            -- blockType=0 for Y AC (i16-AC per libwebp convention)
            (e', hasNz) <- encodeCoefficients residuals coeffProbs 0 ctx 1 e
            case mStats of
              Just s -> countCoefficients residuals 0 ctx 1 s
              Nothing -> return ()

            -- Track NZ
            VSM.write nzGrid blockIdx (if hasNz then 1 else 0)

            encodeYBlock (blockIdx + 1) e' (anyAcNz || hasNz)

  (enc2, anyAcNz) <- encodeYBlock 0 enc1 False

  -- Update aboveNzY with bottom row NZ (blocks 12, 13, 14, 15)
  nz12 <- VSM.read nzGrid 12
  nz13 <- VSM.read nzGrid 13
  nz14 <- VSM.read nzGrid 14
  nz15 <- VSM.read nzGrid 15
  VSM.write aboveNzY (mbX * 4) nz12
  VSM.write aboveNzY (mbX * 4 + 1) nz13
  VSM.write aboveNzY (mbX * 4 + 2) nz14
  VSM.write aboveNzY (mbX * 4 + 3) nz15

  -- Get right column NZ (blocks 3, 7, 11, 15) for next MB's left
  newLeftY0' <- fromIntegral <$> VSM.read nzGrid 3
  newLeftY1' <- fromIntegral <$> VSM.read nzGrid 7
  newLeftY2' <- fromIntegral <$> VSM.read nzGrid 11
  newLeftY3' <- fromIntegral <$> VSM.read nzGrid 15

  -- Now reconstruct all Y blocks for future predictions
  forM_ [0 .. 15] $ \blockIdx -> do
    let subX = (blockIdx `mod` 4) * 4
        subY = (blockIdx `div` 4) * 4

    -- Read trellis-quantized coefficients (stored during encoding pass above)
    residuals <- VSM.new 16
    forM_ [0 .. 15] $ \i -> do
      q <- VSM.unsafeRead residualBlocks (blockIdx * 16 + i)
      VSM.unsafeWrite residuals i q

    -- Dequantize (already quantized by trellis)
    dequantizeBlock dequantFactors 0 residuals

    -- Add reconstructed DC from Y2 (after dequant, before IDCT)
    let reconDC = reconY2DCs VS.! blockIdx
    VSM.write residuals 0 reconDC

    -- IDCT
    idct4x4 residuals

    -- Add to prediction
    forM_ [0 .. 3] $ \row ->
      forM_ [0 .. 3] $ \col -> do
        let px = x + subX + col
            py = y + subY + row
            idx = py * stride + px
        pred <- VSM.read predBuf idx
        res <- VSM.read residuals (row * 4 + col)
        let reconstructed = clip255 (fromIntegral pred + fromIntegral res)
        VSM.write yRecon idx reconstructed

  let !anyYNz = y2nz || anyAcNz
  return (enc2, y2nz, anyYNz, newLeftY0', newLeftY1', newLeftY2', newLeftY3')

-- | Encode Y blocks for a B_PRED macroblock.
-- No Y2 block. Each 4x4 sub-block uses blockType=3 (full coefficients with DC).
-- Predicts each sub-block sequentially from yRecon (which already has B_PRED reconstruction).
-- Returns: (coeffEncoder, leftNzY0..3)
encodeYBlocksBPred ::
  VSM.MVector s Word8 -> -- Y original
  VSM.MVector s Word8 -> -- Y reconstruction
  Int -> -- Stride
  Int ->
  Int -> -- X, Y position
  VS.Vector Word8 -> -- 16 sub-block modes
  DequantFactors ->
  Int -> -- RDO lambda for trellis quantization
  Int -> -- Activity scale (8.8 fixed, 256 = unity) for trellis lambda masking
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder -> -- Coefficient encoder (DCT partition)
  VSM.MVector s Word8 -> -- aboveNzY (mbCols*4)
  Int -> -- mbX
  Int ->
  Int ->
  Int ->
  Int -> -- leftNzY[0..3]
  Maybe (CoeffStats s) -> -- Optional coefficient statistics
  ST s (BoolEncoder, Bool, Int, Int, Int, Int)
  -- Returns: (encoder, anyYNz, leftNzY0..3)
encodeYBlocksBPred yOrig yRecon stride x y bpredModes dequantFactors lambda actScale coeffProbs enc aboveNzY mbX leftNzY0 leftNzY1 leftNzY2 leftNzY3 mStats = do
  -- NZ tracking grid for 16 sub-blocks
  nzGrid <- VSM.replicate 16 (0 :: Word8)

  -- Read above NZ for this MB's Y columns
  aNzCol0 <- VSM.read aboveNzY (mbX * 4)
  aNzCol1 <- VSM.read aboveNzY (mbX * 4 + 1)
  aNzCol2 <- VSM.read aboveNzY (mbX * 4 + 2)
  aNzCol3 <- VSM.read aboveNzY (mbX * 4 + 3)

  -- Process 16 blocks in raster order
  let encodeBlock !blockIdx !e !anyBlockNz
        | blockIdx >= 16 = return (e, anyBlockNz)
        | otherwise = do
            let !row = blockIdx `div` 4
                !col = blockIdx `mod` 4
                !subX = col * 4
                !subY = row * 4
                !mode = fromIntegral (bpredModes VS.! blockIdx) :: Int

            -- Predict into yRecon (reads from already-reconstructed neighbors)
            predict4x4 mode yRecon stride (x + subX) (y + subY)

            -- Compute residuals
            residuals <- VSM.new 16
            forM_ [0 .. 3] $ \r ->
              forM_ [0 .. 3] $ \c -> do
                let !idx = (y + subY + r) * stride + (x + subX + c)
                !orig <- VSM.read yOrig idx
                !pred <- VSM.read yRecon idx
                VSM.write residuals (r * 4 + c) (fromIntegral orig - fromIntegral pred :: Int16)

            -- Forward DCT
            fdct4x4 residuals

            -- Get NZ context
            aboveNz <-
              if row == 0
                then return $ fromIntegral $ case col of
                  0 -> aNzCol0
                  1 -> aNzCol1
                  2 -> aNzCol2
                  _ -> aNzCol3
                else fromIntegral <$> VSM.read nzGrid ((row - 1) * 4 + col)
            leftNz <-
              if col == 0
                then return $ case row of
                  0 -> leftNzY0
                  1 -> leftNzY1
                  2 -> leftNzY2
                  _ -> leftNzY3
                else fromIntegral <$> VSM.read nzGrid (row * 4 + col - 1)
            let !ctx = min 2 (aboveNz + leftNz)

            -- Trellis-quantize (blockType=3: Y full with DC, SSIM-weighted)
            !bpVar256 <- blockOrigVar256 yOrig stride (x + subX) (y + subY)
            let !bpSsScale = ssimTrellisScale bpVar256
            _ <- trellisQuantizeBlock dequantFactors 3 residuals coeffProbs ctx 0 bpSsScale actScale

            -- Encode coefficients (blockType=3 for i4-AC, startPos=0 to include DC)
            (e', hasNz) <- encodeCoefficients residuals coeffProbs 3 ctx 0 e
            case mStats of
              Just s -> countCoefficients residuals 3 ctx 0 s
              Nothing -> return ()
            VSM.write nzGrid blockIdx (if hasNz then 1 else 0)

            -- Dequantize and reconstruct
            dequantizeBlock dequantFactors 3 residuals
            idct4x4 residuals
            forM_ [0 .. 3] $ \r ->
              forM_ [0 .. 3] $ \c -> do
                let !idx = (y + subY + r) * stride + (x + subX + c)
                !pred <- VSM.read yRecon idx
                !res <- VSM.read residuals (r * 4 + c)
                VSM.write yRecon idx (clip255 (fromIntegral pred + fromIntegral res))

            encodeBlock (blockIdx + 1) e' (anyBlockNz || hasNz)

  (enc', anyYNz) <- encodeBlock 0 enc False

  -- Update aboveNzY with bottom row NZ
  nz12 <- VSM.read nzGrid 12
  nz13 <- VSM.read nzGrid 13
  nz14 <- VSM.read nzGrid 14
  nz15 <- VSM.read nzGrid 15
  VSM.write aboveNzY (mbX * 4) nz12
  VSM.write aboveNzY (mbX * 4 + 1) nz13
  VSM.write aboveNzY (mbX * 4 + 2) nz14
  VSM.write aboveNzY (mbX * 4 + 3) nz15

  -- Right column NZ for next MB's left
  newLeftY0' <- fromIntegral <$> VSM.read nzGrid 3
  newLeftY1' <- fromIntegral <$> VSM.read nzGrid 7
  newLeftY2' <- fromIntegral <$> VSM.read nzGrid 11
  newLeftY3' <- fromIntegral <$> VSM.read nzGrid 15

  return (enc', anyYNz, newLeftY0', newLeftY1', newLeftY2', newLeftY3')

-- | Encode chroma blocks (U or V) with NZ context tracking
-- coeffBlockType: 2 for both U and V per RFC 6386 coefficient probability indexing
-- Dequantization always uses type 2 (UV) for both U and V
-- Block layout (2x2): [0][1] / [2][3]
-- Updates aboveNz with bottom row NZ, returns right column NZ
encodeChromaBlocks ::
  VSM.MVector s Word8 -> -- Chroma original (U or V)
  VSM.MVector s Word8 -> -- Chroma reconstruction
  Int -> -- Stride
  Int ->
  Int -> -- X, Y position
  Int -> -- Prediction mode (0-3)
  DequantFactors ->
  Int -> -- RDO lambda for trellis quantization
  Int -> -- Activity scale (8.8 fixed, 256 = unity) for trellis lambda masking
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  Int -> -- Coefficient block type (2 for both U and V)
  VSM.MVector s Word8 -> -- aboveNz (mbCols*2, read top row, write bottom row)
  Int -> -- mbX
  Int ->
  Int -> -- leftNz row 0, row 1
  Maybe (CoeffStats s) -> -- Optional coefficient statistics
  ST s (BoolEncoder, Bool, Int, Int)
  -- Returns: (encoder, anyChromaNz, leftNz0, leftNz1)
encodeChromaBlocks chromaOrig chromaRecon stride x y predMode dequantFactors lambda actScale coeffProbs enc coeffBlockType aboveNz mbX leftNz0 leftNz1 mStats = do
  -- Create temporary buffer for prediction
  predBuf <- VSM.clone chromaRecon

  -- Apply prediction to temporary buffer
  predict8x8 predMode predBuf stride x y

  -- Read above NZ for this MB's chroma columns
  aNzCol0 <- VSM.read aboveNz (mbX * 2)
  aNzCol1 <- VSM.read aboveNz (mbX * 2 + 1)

  -- NZ tracking grid for 4 blocks (2x2)
  nzGrid <- VSM.replicate 4 (0 :: Word8)

  -- Process 4 chroma blocks in raster order with NZ context
  let processBlock !blockIdx !e !anyBlockNz
        | blockIdx >= 4 = return (e, anyBlockNz)
        | otherwise = do
            let !row = blockIdx `div` 2
                !col = blockIdx `mod` 2
                subX = col * 4
                subY = row * 4

            -- Get above NZ
            aboveNzVal <-
              if row == 0
                then return $ fromIntegral $ if col == 0 then aNzCol0 else aNzCol1
                else fromIntegral <$> VSM.read nzGrid col -- block above: row 0, same col

            -- Get left NZ
            leftNzVal <-
              if col == 0
                then return $ if row == 0 then leftNz0 else leftNz1
                else fromIntegral <$> VSM.read nzGrid (row * 2) -- block to the left
            let !ctx = min 2 (aboveNzVal + leftNzVal)

            -- Allocate residual block
            residuals <- VSM.new 16

            -- Compute residuals
            forM_ [0 .. 3] $ \r ->
              forM_ [0 .. 3] $ \c -> do
                let px = x + subX + c
                    py = y + subY + r
                    idx = py * stride + px
                orig <- VSM.read chromaOrig idx
                pred <- VSM.read predBuf idx
                let residual = fromIntegral orig - fromIntegral pred :: Int16
                VSM.write residuals (r * 4 + c) residual

            -- Forward DCT
            fdct4x4 residuals

            -- SSIM-aware trellis scale from chroma block variance: flat blocks
            -- get full distortion weight (preserve DC accuracy), textured blocks
            -- allow more aggressive zeroing (masked by visual complexity).
            !cVar256 <- blockOrigVar256 chromaOrig stride (x + subX) (y + subY)
            let !cSsScale = ssimTrellisScale cVar256

            -- Trellis-quantize (always use type 2 = UV quant for both U and V)
            _ <- trellisQuantizeBlock dequantFactors 2 residuals coeffProbs ctx 0 cSsScale actScale

            -- Encode coefficients with NZ context
            (e', hasNz) <- encodeCoefficients residuals coeffProbs coeffBlockType ctx 0 e
            case mStats of
              Just s -> countCoefficients residuals coeffBlockType ctx 0 s
              Nothing -> return ()

            -- Track NZ
            VSM.write nzGrid blockIdx (if hasNz then 1 else 0)

            -- Reconstruct (always use type 2 = UV dequant for both U and V)
            dequantizeBlock dequantFactors 2 residuals
            idct4x4 residuals

            -- Add prediction back and write to reconstruction buffer
            forM_ [0 .. 3] $ \r ->
              forM_ [0 .. 3] $ \c -> do
                let px = x + subX + c
                    py = y + subY + r
                    idx = py * stride + px
                pred <- VSM.read predBuf idx
                res <- VSM.read residuals (r * 4 + c)
                let reconstructed = clip255 (fromIntegral pred + fromIntegral res)
                VSM.write chromaRecon idx reconstructed
            processBlock (blockIdx + 1) e' (anyBlockNz || hasNz)

  (enc', anyChromaNz) <- processBlock 0 enc False

  -- Update aboveNz with bottom row NZ (blocks 2 and 3)
  nz2 <- VSM.read nzGrid 2
  nz3 <- VSM.read nzGrid 3
  VSM.write aboveNz (mbX * 2) nz2
  VSM.write aboveNz (mbX * 2 + 1) nz3

  -- Return right column NZ (blocks 1 and 3) for next MB's left
  newLeft0 <- fromIntegral <$> VSM.read nzGrid 1
  newLeft1 <- fromIntegral <$> VSM.read nzGrid 3

  return (enc', anyChromaNz, newLeft0, newLeft1)

-- ---------------------------------------------------------------------------
-- Spatial Noise Shaping (SNS) segmentation
-- ---------------------------------------------------------------------------

-- | Compute coding complexity for a single 16x16 macroblock.
-- Uses per-sub-block SAD analysis to distinguish edges from texture:
--   - Edges have concentrated prediction errors (few sub-blocks dominate)
--     → reduced alpha → assigned to finer-QP segments
--   - Texture has spread prediction errors (many sub-blocks contribute)
--     → full alpha → assigned to coarser-QP segments (errors are masked)
{-# INLINE computeMBAlpha #-}
computeMBAlpha :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s Int
computeMBAlpha buf stride bx by = do
  -- Read first row (V prediction reference) and first column (H prediction)
  firstRow <- VUM.new 16
  firstCol <- VUM.new 16
  forM_ [0 .. 15] $ \i -> do
    !topPx <- fromIntegral <$> VSM.unsafeRead buf (by * stride + bx + i)
    VUM.unsafeWrite firstRow i (topPx :: Int)
    !leftPx <- fromIntegral <$> VSM.unsafeRead buf ((by + i) * stride + bx)
    VUM.unsafeWrite firstCol i (leftPx :: Int)
  -- Pass 1: compute block mean for DC prediction
  let goMean !i !s
        | i >= 256 = return $! (s + 128) `div` 256
        | otherwise = do
            let !r = i `shiftR` 4
                !c = i .&. 15
            !px <- fromIntegral <$> VSM.unsafeRead buf ((by + r) * stride + (bx + c))
            goMean (i + 1) (s + px)
  !dcPred <- goMean 0 (0 :: Int)
  -- Pass 2: compute per-sub-block SADs for DC, H, V predictions
  -- Each macroblock has 16 sub-blocks (4x4 pixels each)
  subSadDC <- VUM.replicate 16 (0 :: Int)
  subSadH <- VUM.replicate 16 (0 :: Int)
  subSadV <- VUM.replicate 16 (0 :: Int)
  let goSAD !i
        | i >= 256 = return ()
        | otherwise = do
            let !r = i `shiftR` 4
                !c = i .&. 15
                !blk = (r `shiftR` 2) `shiftL` 2 + (c `shiftR` 2)
            !px <- fromIntegral <$> VSM.unsafeRead buf ((by + r) * stride + (bx + c))
            !hRef <- VUM.unsafeRead firstCol r
            !vRef <- VUM.unsafeRead firstRow c
            VUM.unsafeModify subSadDC (+ abs (px - dcPred)) blk
            VUM.unsafeModify subSadH (+ abs (px - hRef)) blk
            VUM.unsafeModify subSadV (+ abs (px - vRef)) blk
            goSAD (i + 1)
  goSAD 0
  -- Sum per-mode totals
  let sumVec !v = do
        let go !i !acc
              | i >= 16 = return acc
              | otherwise = do
                  !x <- VUM.unsafeRead v i
                  go (i + 1) (acc + x)
        go 0 0
  !totalDC <- sumVec subSadDC
  !totalH <- sumVec subSadH
  !totalV <- sumVec subSadV
  -- Find best prediction mode
  let !bestTotal = min totalDC (min totalH totalV)
  if bestTotal == 0
    then return 0
    else do
      let !bestSubs
            | totalDC <= totalH && totalDC <= totalV = subSadDC
            | totalH <= totalV = subSadH
            | otherwise = subSadV
      -- Count "active" sub-blocks: those with SAD above half the mean.
      -- Edges: few active (concentrated errors) → lower effective alpha
      -- Texture: many active (spread errors) → full alpha
      let !thresh = max 1 (bestTotal `div` 32)
      let countActive !i !cnt
            | i >= 16 = return cnt
            | otherwise = do
                !s <- VUM.unsafeRead bestSubs i
                countActive (i + 1) (if s >= thresh then cnt + 1 else cnt)
      !numActive <- countActive 0 (0 :: Int)
      -- Scale alpha by texture ratio:
      --   numActive=0 (pure edge): alpha = bestTotal * 16/32 = bestTotal/2
      --   numActive=16 (pure texture): alpha = bestTotal * 32/32 = bestTotal
      return $! bestTotal * (16 + numActive) `div` 32

-- | Compute per-MB complexity scores for all macroblocks.
computeMBAlphas ::
  VSM.MVector s Word8 -> -- Y buffer
  Int -> -- Stride (padded width)
  Int -> -- MB rows
  Int -> -- MB cols
  ST s (VU.Vector Int)
computeMBAlphas yBuf stride mbRows mbCols = do
  let !n = mbRows * mbCols
  result <- VUM.new n
  let go !i
        | i >= n = VU.unsafeFreeze result
        | otherwise = do
            let !r = i `div` mbCols
                !c = i - r * mbCols
            !alpha <- computeMBAlpha yBuf stride (c * 16) (r * 16)
            VUM.unsafeWrite result i alpha
            go (i + 1)
  go 0

-- | Compute per-MB activity weights for perceptual RDO lambda modulation.
-- Returns 8.8 fixed-point weights (256 = 1.0): smooth MBs get lower weight
-- (lower effective lambda → preserve quality), busy MBs get higher weight
-- (higher effective lambda → accept more distortion where it's masked).
computeActivityWeights :: VU.Vector Int -> Int -> VU.Vector Int
computeActivityWeights alphas mbCount
  | mbCount == 0 = VU.empty
  | otherwise =
      let !avgAlpha = max 1 (VU.foldl' (+) 0 alphas `div` mbCount)
       in VU.map (\a -> max 128 $ min 512 $ (256 * a) `div` max 1 avgAlpha) alphas

-- | Classify macroblocks into 4 segments using k-means clustering and compute
-- adaptive QI deltas from centroid positions (Spatial Noise Shaping).
--
-- Unlike simple variance-quartile classification, k-means finds natural
-- complexity clusters and computes QI deltas proportional to each cluster's
-- deviation from the weighted mean — automatically shrinking deltas when
-- the image has uniform complexity.
--
-- Returns (segment map, QI deltas, counts per segment).
-- Segments are ordered: 0 = smoothest, 3 = busiest.
classifySegmentsSNS :: VU.Vector Int -> Int -> (VU.Vector Word8, VU.Vector Int, Int, Int, Int, Int)
classifySegmentsSNS alphas qi =
  let !n = VU.length alphas
      !minA = VU.minimum alphas
      !maxA = VU.maximum alphas
      !rangeA = maxA - minA
   in if rangeA < 2
        then -- Uniform image: no benefit from segmentation
          (VU.replicate n 0, VU.fromList [0, 0, 0, 0], n, 0, 0, 0)
        else
          let !numBins = 256 :: Int

              -- Map alpha value to bin index [0, numBins-1]
              toBin a = min (numBins - 1) $ ((a - minA) * (numBins - 1)) `div` rangeA

              -- Build histogram via mutable vector
              !hist = VU.create $ do
                h <- VUM.replicate numBins (0 :: Int)
                let bld !i
                      | i >= n = return h
                      | otherwise = do
                          VUM.unsafeModify h (+ 1) (toBin (alphas VU.! i))
                          bld (i + 1)
                bld 0

              -- One k-means iteration on the histogram
              kMeansStep (!c0, !c1, !c2, !c3) =
                let nearest !bin
                      | d0 <= d1 && d0 <= d2 && d0 <= d3 = 0 :: Int
                      | d1 <= d2 && d1 <= d3 = 1
                      | d2 <= d3 = 2
                      | otherwise = 3
                      where
                        !d0 = abs (bin - c0)
                        !d1 = abs (bin - c1)
                        !d2 = abs (bin - c2)
                        !d3 = abs (bin - c3)
                    accum !bin !s0 !s1 !s2 !s3 !n0 !n1 !n2 !n3
                      | bin >= numBins = (s0, s1, s2, s3, n0, n1, n2, n3)
                      | otherwise =
                          let !cnt = hist VU.! bin
                           in if cnt == 0
                                then accum (bin + 1) s0 s1 s2 s3 n0 n1 n2 n3
                                else case nearest bin of
                                  0 -> accum (bin + 1) (s0 + cnt * bin) s1 s2 s3 (n0 + cnt) n1 n2 n3
                                  1 -> accum (bin + 1) s0 (s1 + cnt * bin) s2 s3 n0 (n1 + cnt) n2 n3
                                  2 -> accum (bin + 1) s0 s1 (s2 + cnt * bin) s3 n0 n1 (n2 + cnt) n3
                                  _ -> accum (bin + 1) s0 s1 s2 (s3 + cnt * bin) n0 n1 n2 (n3 + cnt)
                    (!s0, !s1, !s2, !s3, !n0, !n1, !n2, !n3) =
                      accum 0 0 0 0 0 0 0 0 0
                 in ( if n0 > 0 then (s0 + n0 `div` 2) `div` n0 else c0
                    , if n1 > 0 then (s1 + n1 `div` 2) `div` n1 else c1
                    , if n2 > 0 then (s2 + n2 `div` 2) `div` n2 else c2
                    , if n3 > 0 then (s3 + n3 `div` 2) `div` n3 else c3
                    )

              -- 6 iterations (matching libwebp MAX_ITERS_K_MEANS)
              runKMeans !cs !iters
                | iters <= (0 :: Int) = cs
                | otherwise = runKMeans (kMeansStep cs) (iters - 1)

              (!fc0, !fc1, !fc2, !fc3) =
                runKMeans
                  ( numBins `div` 8, 3 * numBins `div` 8
                  , 5 * numBins `div` 8, 7 * numBins `div` 8
                  )
                  6

              -- Sort centers: segment 0 = lowest alpha = smoothest
              sorted = sort [fc0, fc1, fc2, fc3]
              !sc0 = sorted !! 0
              !sc1 = sorted !! 1
              !sc2 = sorted !! 2
              !sc3 = sorted !! 3

              -- Assign each MB to nearest sorted center
              assign a =
                let !bin = toBin a
                    !d0 = abs (bin - sc0)
                    !d1 = abs (bin - sc1)
                    !d2 = abs (bin - sc2)
                    !d3 = abs (bin - sc3)
                 in if d0 <= d1 && d0 <= d2 && d0 <= d3
                      then 0
                      else
                        if d1 <= d2 && d1 <= d3
                          then 1
                          else if d2 <= d3 then 2 else 3 :: Word8

              segMap = VU.map assign alphas
              !cnt0 = VU.foldl' (\acc s -> if s == 0 then acc + 1 else acc) 0 segMap
              !cnt1 = VU.foldl' (\acc s -> if s == 1 then acc + 1 else acc) 0 segMap
              !cnt2 = VU.foldl' (\acc s -> if s == 2 then acc + 1 else acc) 0 segMap
              !cnt3 = n - cnt0 - cnt1 - cnt2

              -- Convert bin centers to alpha space for delta computation
              toAlpha bin = minA + (bin * rangeA + (numBins `div` 2)) `div` numBins
              !a0 = toAlpha sc0
              !a1 = toAlpha sc1
              !a2 = toAlpha sc2
              !a3 = toAlpha sc3

              -- Weighted mean alpha (reference point: delta = 0 for average complexity)
              !totalCnt = max 1 (cnt0 + cnt1 + cnt2 + cnt3)
              !meanAlpha = (a0 * cnt0 + a1 * cnt1 + a2 * cnt2 + a3 * cnt3) `div` totalCnt

              -- Max deviation from mean (for scaling)
              !maxDev =
                max 1 $
                  maximum
                    [ abs (a0 - meanAlpha),
                      abs (a1 - meanAlpha),
                      abs (a2 - meanAlpha),
                      abs (a3 - meanAlpha)
                    ]

              -- Max QI delta: ~15% of qi (similar magnitude to previous fixed deltas)
              !maxDelta = qi * 3 `div` 20

              -- Per-segment delta proportional to deviation from mean
              -- Smooth (below mean) → negative delta → finer quantization
              -- Busy (above mean) → positive delta → coarser quantization
              computeDelta a =
                let !dev = a - meanAlpha
                    !delta = (dev * maxDelta) `quot` maxDev
                 in max (negate qi) $ min (127 - qi) delta

              segDeltas =
                VU.fromList
                  [computeDelta a0, computeDelta a1, computeDelta a2, computeDelta a3]
           in (segMap, segDeltas, cnt0, cnt1, cnt2, cnt3)

-- | Compute per-segment filter strength deltas from quantizer deltas.
-- Each segment's filter level should match its effective quantization:
-- since base filter level ≈ qi/2, the filter delta tracks the QP delta.
-- Coarser quantization (higher qi) → more blocking → stronger filter.
-- Finer quantization (lower qi) → less blocking → weaker filter.
computeSegmentFilterDeltas :: Int -> VU.Vector Int -> VU.Vector Int
computeSegmentFilterDeltas qi qDeltas =
  let !baseLevel = min 63 $ max 0 $ qi `div` 2
   in VU.map
        ( \qd ->
            let !effectiveQi = max 0 $ min 127 $ qi + qd
                !desiredLevel = min 63 $ max 0 $ effectiveQi `div` 2
             in max (-63) $ min 63 $ desiredLevel - baseLevel
        )
        qDeltas

-- | Compute 3 segment tree probabilities from segment counts.
-- Balanced tree (matching libwebp): prob[0] splits {seg0,seg1} vs {seg2,seg3},
-- prob[1] splits seg0 vs seg1, prob[2] splits seg2 vs seg3.
-- Probability = P(go left / bit=False) = round(256 * count_left / total_at_node), clamped to [1,255].
computeSegmentProbs :: Int -> Int -> Int -> Int -> (Word8, Word8, Word8)
computeSegmentProbs c0 c1 c2 c3 =
  let total = c0 + c1 + c2 + c3
      clampProb p = fromIntegral (max 1 (min 255 p)) :: Word8
      -- prob[0]: P(seg in {0,1}) = (c0+c1)/total
      !left01 = c0 + c1
      !p0 = clampProb $ if total > 0 then (256 * left01 + total `div` 2) `div` total else 255
      -- prob[1]: P(seg=0 | seg in {0,1}) = c0/(c0+c1)
      !p1 = clampProb $ if left01 > 0 then (256 * c0 + left01 `div` 2) `div` left01 else 255
      -- prob[2]: P(seg=2 | seg in {2,3}) = c2/(c2+c3)
      !right23 = c2 + c3
      !p2 = clampProb $ if right23 > 0 then (256 * c2 + right23 `div` 2) `div` right23 else 255
   in (p0, p1, p2)

-- | Encode a segment ID (0-3) using the VP8 balanced segment tree.
-- Tree (matching libwebp): prob[0] splits {0,1} vs {2,3},
-- prob[1] splits 0 vs 1, prob[2] splits 2 vs 3.
-- False = left / 0, True = right / 1.
{-# INLINE encodeSegmentId #-}
encodeSegmentId :: Int -> Word8 -> Word8 -> Word8 -> BoolEncoder -> BoolEncoder
encodeSegmentId 0 p0 p1 _ enc =
  let !e1 = boolWrite p0 False enc -- left: {0,1}
   in boolWrite p1 False e1 -- left: 0
encodeSegmentId 1 p0 p1 _ enc =
  let !e1 = boolWrite p0 False enc -- left: {0,1}
   in boolWrite p1 True e1 -- right: 1
encodeSegmentId 2 p0 _ p2 enc =
  let !e1 = boolWrite p0 True enc -- right: {2,3}
   in boolWrite p2 False e1 -- left: 2
encodeSegmentId _ p0 _ p2 enc =
  let !e1 = boolWrite p0 True enc -- right: {2,3}
   in boolWrite p2 True e1 -- right: 3
