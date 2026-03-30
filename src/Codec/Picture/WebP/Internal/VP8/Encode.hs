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
import Codec.Picture.WebP.Internal.VP8.ColorConvert
import Codec.Picture.WebP.Internal.VP8.DCT
import Codec.Picture.WebP.Internal.VP8.Dequant
import Codec.Picture.WebP.Internal.VP8.EncodeCoefficients
import Codec.Picture.WebP.Internal.VP8.EncodeHeader
import Codec.Picture.WebP.Internal.VP8.EncodeMode
import Codec.Picture.WebP.Internal.VP8.FilterStrengthSearch (optimizeFilterStrength)
import Codec.Picture.WebP.Internal.VP8.IDCT
import Codec.Picture.WebP.Internal.VP8.LoopFilter (applyNormalLoopFilterRow)
import Codec.Picture.WebP.Internal.VP8.ModeSelection
import Codec.Picture.WebP.Internal.VP8.Predict
import Codec.Picture.WebP.Internal.VP8.Quantize (applySharpen, qualityToYacQi, rdLambdaFromQi, trellisQuantizeBlock)
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
  (yBuf, uBuf, vBuf) <- rgbToYCbCr img

  let width = imageWidth img
      height = imageHeight img
      paddedW = ((width + 15) `div` 16) * 16
      paddedH = ((height + 15) `div` 16) * 16
      mbCols = paddedW `div` 16
      mbRows = paddedH `div` 16

  -- Step 2: Set up encoder configuration
  let config = defaultEncodeConfig quality
      qi = qualityToYacQi quality
      quantIndices =
        QuantIndices
          { qiYacQi = qi,
            qiYdcDelta = 0,
            qiY2dcDelta = 0,
            qiY2acDelta = 0,
            qiUvdcDelta = 0,
            qiUvacDelta = 0
          }

  -- Step 3: Adaptive QP segmentation analysis
  let segDeltas = computeSegmentDeltas qi
      useSegmentation =
        encUseSegmentation config
          && VU.any (/= 0) segDeltas
          && mbRows * mbCols >= 4

  (mSegHeaderInfo, dequantFactorsVec, segLambdas, mSegEncInfo) <-
    if useSegmentation
      then do
        variances <- computeMBVariances yBuf paddedW mbRows mbCols
        let (segMap, c0, c1, c2, c3) = classifySegments variances
            (sp0, sp1, sp2) = computeSegmentProbs c0 c1 c2 c3
            segInfo =
              SegmentInfo
                { segmentEnabled = True,
                  segmentUpdateMap = True,
                  segmentAbsoluteMode = False,
                  segmentQuantizer = segDeltas,
                  segmentFilterStrength = VU.fromList [0, 0, 0, 0],
                  segmentTreeProbs = (sp0, sp1, sp2)
                }
            dqVec = computeDequantFactors quantIndices (Just segInfo)
            lams =
              VU.generate 4 $ \s ->
                rdLambdaFromQi (max 0 (min 127 (qi + segDeltas VU.! s)))
        return (Just (segInfo, sp0, sp1, sp2), dqVec, lams, Just (segMap, sp0, sp1, sp2))
      else do
        let dqVec = computeDequantFactors quantIndices Nothing
            lams = VU.singleton (rdLambdaFromQi qi)
        return (Nothing, dqVec, lams, Nothing)

  -- Step 4: Allocate reconstruction buffers (for prediction)
  let !ySize = paddedW * paddedH
      !uvSize = (paddedW `div` 2) * (paddedH `div` 2)
  yRecon <- VSM.replicate ySize 128
  uRecon <- VSM.replicate uvSize 128
  vRecon <- VSM.replicate uvSize 128

  -- Step 5: Pass 1 — encode with default probs, collect coefficient statistics
  --         Also save pre-filter reconstruction for filter strength search
  stats <- newCoeffStats
  let noUpdateFlags = VU.replicate 1056 False
      defaultFilterLevel = encFilterLevel config
      compressedHeaderEnc1 = generateCompressedHeader quantIndices defaultFilterLevel (encFilterType config) mSegHeaderInfo defaultCoeffProbs noUpdateFlags

  -- Allocate pre-filter buffers (capture reconstruction before loop filter)
  yPreFilter <- VSM.new ySize
  uPreFilter <- VSM.new uvSize
  vPreFilter <- VSM.new uvSize

  (modeEnc1, coeffEnc1) <-
    encodeMacroblocks
      yBuf uBuf vBuf yRecon uRecon vRecon
      paddedW paddedH mbRows mbCols
      dequantFactorsVec segLambdas mSegEncInfo defaultCoeffProbs
      compressedHeaderEnc1 initBoolEncoder
      defaultFilterLevel (Just stats)
      (Just (yPreFilter, uPreFilter, vPreFilter))

  -- Step 6: Adaptive filter strength search
  optFilterLevel <-
    if defaultFilterLevel > 0
      then
        optimizeFilterStrength
          yBuf uBuf vBuf yPreFilter uPreFilter vPreFilter
          paddedW mbRows mbCols defaultFilterLevel
      else return 0

  -- Step 7: Compute optimal coefficient probabilities from statistics
  optimalProbs <- computeOptimalProbs stats
  (updatedProbs, updateFlags) <- decideUpdates stats optimalProbs
  let hasUpdates = VU.any id updateFlags
      needsReencode = hasUpdates || optFilterLevel /= defaultFilterLevel

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

      -- Step 9: Pass 2 — re-encode with optimal filter level and probs
      let probs2 = if hasUpdates then updatedProbs else defaultCoeffProbs
          flags2 = if hasUpdates then updateFlags else noUpdateFlags
          compressedHeaderEnc2 = generateCompressedHeader quantIndices optFilterLevel (encFilterType config) mSegHeaderInfo probs2 flags2

      (modeEnc2, coeffEnc2) <-
        encodeMacroblocks
          yBuf uBuf vBuf yRecon uRecon vRecon
          paddedW paddedH mbRows mbCols
          dequantFactorsVec segLambdas mSegEncInfo probs2
          compressedHeaderEnc2 initBoolEncoder
          optFilterLevel Nothing
          Nothing

      let partition0 = finalizeBoolEncoder modeEnc2
          dctPartition = finalizeBoolEncoder coeffEnc2
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
  Maybe (VU.Vector Word8, Word8, Word8, Word8) -> -- Segment map + 3 tree probs (Nothing = no segments)
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder -> -- Mode encoder (partition 0)
  BoolEncoder -> -- Coefficient encoder (DCT partition)
  Int -> -- Filter level for per-row loop filter
  Maybe (CoeffStats s) -> -- Optional coefficient statistics accumulator
  Maybe (VSM.MVector s Word8, VSM.MVector s Word8, VSM.MVector s Word8) -> -- Pre-filter buffers (save recon before loop filter)
  ST s (BoolEncoder, BoolEncoder)
encodeMacroblocks yOrig uOrig vOrig yRecon uRecon vRecon paddedW paddedH mbRows mbCols dqVec segLambdas mSegEncInfo coeffProbs modeEnc coeffEnc filterLevel mStats mPreFilterBufs = do
  -- Allocate above NZ tracking arrays (persist across MB rows)
  aboveNzY <- VSM.replicate (mbCols * 4) (0 :: Word8) -- 4 Y columns per MB
  aboveNzU <- VSM.replicate (mbCols * 2) (0 :: Word8) -- 2 U columns per MB
  aboveNzV <- VSM.replicate (mbCols * 2) (0 :: Word8) -- 2 V columns per MB
  aboveNzDC <- VSM.replicate mbCols (0 :: Word8) -- 1 DC per MB
  -- B_PRED sub-block mode context (4 modes per MB column for bottom row)
  -- Non-B_PRED MBs store 0 (B_DC_PRED) as default context
  aboveBModes <- VSM.replicate (mbCols * 4) (0 :: Word8)
  let loop !mbY !mbX !mEnc !cEnc !leftNzY0 !leftNzY1 !leftNzY2 !leftNzY3 !leftNzU0 !leftNzU1 !leftNzV0 !leftNzV1 !leftNzDC !leftBM0 !leftBM1 !leftBM2 !leftBM3
        | mbY >= mbRows = return (mEnc, cEnc)
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
              applyNormalLoopFilterRow yRecon paddedW uRecon (paddedW `div` 2) vRecon (paddedW `div` 2) mbY mbCols filterLevel
            -- New row: reset left NZ and left B modes to 0
            loop (mbY + 1) 0 mEnc cEnc 0 0 0 0 0 0 0 0 0 0 0 0 0
        | otherwise = do
            -- Segment handling: look up per-MB segment, write ID, select per-segment params
            let (!segDq, !segLam, !mEncSeg) = case mSegEncInfo of
                  Nothing ->
                    (dqVec V.! 0, segLambdas VU.! 0, mEnc)
                  Just (segMap, sp0, sp1, sp2) ->
                    let !s = fromIntegral (segMap VU.! (mbY * mbCols + mbX))
                     in (dqVec V.! s, segLambdas VU.! s, encodeSegmentId s sp0 sp1 sp2 mEnc)

            (mEnc', cEnc', lY0, lY1, lY2, lY3, lU0, lU1, lV0, lV1, lDC, lBM0, lBM1, lBM2, lBM3) <-
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
                segLam
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
            loop mbY (mbX + 1) mEnc' cEnc' lY0 lY1 lY2 lY3 lU0 lU1 lV0 lV1 lDC lBM0 lBM1 lBM2 lBM3

  loop 0 0 modeEnc coeffEnc 0 0 0 0 0 0 0 0 0 0 0 0 0

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
  Int -> -- RDO lambda
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
  ST s (BoolEncoder, BoolEncoder, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
encodeMacroblock yOrig uOrig vOrig yRecon uRecon vRecon paddedW _paddedH mbY mbX dequantFactors lambda coeffProbs mEnc cEnc aboveNzY aboveNzU aboveNzV aboveNzDC aboveBModes leftNzY0 leftNzY1 leftNzY2 leftNzY3 leftNzU0 leftNzU1 leftNzV0 leftNzV1 leftNzDC leftBM0 leftBM1 leftBM2 leftBM3 mStats = do
  let mbXpix = mbX * 16
      mbYpix = mbY * 16

  -- Read above B-modes early (needed for B_PRED RDO and encoding)
  aBM0 <- VSM.read aboveBModes (mbX * 4)
  aBM1 <- VSM.read aboveBModes (mbX * 4 + 1)
  aBM2 <- VSM.read aboveBModes (mbX * 4 + 2)
  aBM3 <- VSM.read aboveBModes (mbX * 4 + 3)

  -- Step 1: Select best i16 Y mode using RDO
  (i16Mode, i16Cost) <- selectIntra16x16ModeRDO yOrig yRecon paddedW mbXpix mbYpix dequantFactors lambda coeffProbs

  -- Step 2: Select best B_PRED modes using RDO (modifies yRecon's MB area)
  (bpredModes, bpredCost) <- selectBPredModeRDO yOrig yRecon paddedW mbXpix mbYpix dequantFactors lambda coeffProbs (fromIntegral aBM0) (fromIntegral aBM1) (fromIntegral aBM2) (fromIntegral aBM3) leftBM0 leftBM1 leftBM2 leftBM3
  -- yRecon now has B_PRED reconstruction; if i16 wins, encodeYBlocks will overwrite it

  -- True RDO: mode encoding costs already included in i16Cost and bpredCost
  let useBPred = bpredCost < i16Cost

  -- Step 3: Select best UV mode using RDO (both U and V)
  let chromaX = mbX * 8
      chromaY = mbY * 8
  (uvPredMode, _) <- selectChromaModeRDO uOrig uRecon vOrig vRecon (paddedW `div` 2) chromaX chromaY dequantFactors lambda coeffProbs

  if useBPred
    then do
      -- === B_PRED path ===
      -- Write B_PRED Y mode to partition 0
      let mEnc1 = encodeYModeBPred mEnc
          -- aBM0..3 already read above for RDO
          mEnc2 = encodeBPredModesToStream bpredModes aBM0 aBM1 aBM2 aBM3 leftBM0 leftBM1 leftBM2 leftBM3 mEnc1
          mEnc3 = encodeUVMode uvPredMode mEnc2

      -- Encode Y blocks (B_PRED: blockType=3, no Y2)
      (cEnc1, newLeftY0, newLeftY1, newLeftY2, newLeftY3) <-
        encodeYBlocksBPred
          yOrig
          yRecon
          paddedW
          mbXpix
          mbYpix
          bpredModes
          dequantFactors
          lambda
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
      (cEnc2, newLeftU0, newLeftU1) <-
        encodeChromaBlocks
          uOrig
          uRecon
          (paddedW `div` 2)
          chromaX
          chromaY
          uvPredMode
          dequantFactors
          lambda
          coeffProbs
          cEnc1
          2
          aboveNzU
          mbX
          leftNzU0
          leftNzU1
          mStats

      (cEnc3, newLeftV0, newLeftV1) <-
        encodeChromaBlocks
          vOrig
          vRecon
          (paddedW `div` 2)
          chromaX
          chromaY
          uvPredMode
          dequantFactors
          lambda
          coeffProbs
          cEnc2
          2
          aboveNzV
          mbX
          leftNzV0
          leftNzV1
          mStats

      -- Right column B modes (blocks 3,7,11,15) for next MB's left context
      let !newLBM0 = fromIntegral (bpredModes VS.! 3)
          !newLBM1 = fromIntegral (bpredModes VS.! 7)
          !newLBM2 = fromIntegral (bpredModes VS.! 11)
          !newLBM3 = fromIntegral (bpredModes VS.! 15)

      return (mEnc3, cEnc3, newLeftY0, newLeftY1, newLeftY2, newLeftY3, newLeftU0, newLeftU1, newLeftV0, newLeftV1, 0, newLBM0, newLBM1, newLBM2, newLBM3)
    else do
      -- === i16 path (original) ===
      let mEnc1 = encodeYMode i16Mode mEnc
          mEnc2 = encodeUVMode uvPredMode mEnc1

      aNzDC <- VSM.read aboveNzDC mbX
      (cEnc1, y2nz, newLeftY0, newLeftY1, newLeftY2, newLeftY3) <-
        encodeYBlocks
          yOrig
          yRecon
          paddedW
          mbXpix
          mbYpix
          i16Mode
          dequantFactors
          lambda
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

      (cEnc2, newLeftU0, newLeftU1) <-
        encodeChromaBlocks
          uOrig
          uRecon
          (paddedW `div` 2)
          chromaX
          chromaY
          uvPredMode
          dequantFactors
          lambda
          coeffProbs
          cEnc1
          2
          aboveNzU
          mbX
          leftNzU0
          leftNzU1
          mStats

      (cEnc3, newLeftV0, newLeftV1) <-
        encodeChromaBlocks
          vOrig
          vRecon
          (paddedW `div` 2)
          chromaX
          chromaY
          uvPredMode
          dequantFactors
          lambda
          coeffProbs
          cEnc2
          2
          aboveNzV
          mbX
          leftNzV0
          leftNzV1
          mStats

      let !newLeftDC = if y2nz then 1 else 0
      return (mEnc2, cEnc3, newLeftY0, newLeftY1, newLeftY2, newLeftY3, newLeftU0, newLeftU1, newLeftV0, newLeftV1, newLeftDC, 0, 0, 0, 0)

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
  ST s (BoolEncoder, Bool, Int, Int, Int, Int)
encodeYBlocks yOrig yRecon stride x y predMode dequantFactors lambda coeffProbs enc aboveNzY mbX leftNzY0 leftNzY1 leftNzY2 leftNzY3 aboveDcNz leftDcNz mStats = do
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
  _ <- trellisQuantizeBlock dequantFactors 1 y2DCs coeffProbs dcCtx 0 lambda

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
  let encodeYBlock !blockIdx !e
        | blockIdx >= 16 = return e
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

            -- Sharpen + trellis-quantize AC coefficients
            applySharpen dequantFactors 0 residuals
            let !ctx = min 2 (aboveNz + leftNz)
            _ <- trellisQuantizeBlock dequantFactors 0 residuals coeffProbs ctx 1 lambda

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

            encodeYBlock (blockIdx + 1) e'

  enc2 <- encodeYBlock 0 enc1

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

  return (enc2, y2nz, newLeftY0', newLeftY1', newLeftY2', newLeftY3')

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
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder -> -- Coefficient encoder (DCT partition)
  VSM.MVector s Word8 -> -- aboveNzY (mbCols*4)
  Int -> -- mbX
  Int ->
  Int ->
  Int ->
  Int -> -- leftNzY[0..3]
  Maybe (CoeffStats s) -> -- Optional coefficient statistics
  ST s (BoolEncoder, Int, Int, Int, Int)
encodeYBlocksBPred yOrig yRecon stride x y bpredModes dequantFactors lambda coeffProbs enc aboveNzY mbX leftNzY0 leftNzY1 leftNzY2 leftNzY3 mStats = do
  -- NZ tracking grid for 16 sub-blocks
  nzGrid <- VSM.replicate 16 (0 :: Word8)

  -- Read above NZ for this MB's Y columns
  aNzCol0 <- VSM.read aboveNzY (mbX * 4)
  aNzCol1 <- VSM.read aboveNzY (mbX * 4 + 1)
  aNzCol2 <- VSM.read aboveNzY (mbX * 4 + 2)
  aNzCol3 <- VSM.read aboveNzY (mbX * 4 + 3)

  -- Process 16 blocks in raster order
  let encodeBlock !blockIdx !e
        | blockIdx >= 16 = return e
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

            -- Sharpen + trellis-quantize (blockType=3: Y full with DC)
            applySharpen dequantFactors 3 residuals
            _ <- trellisQuantizeBlock dequantFactors 3 residuals coeffProbs ctx 0 lambda

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

            encodeBlock (blockIdx + 1) e'

  enc' <- encodeBlock 0 enc

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

  return (enc', newLeftY0', newLeftY1', newLeftY2', newLeftY3')

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
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  Int -> -- Coefficient block type (2 for both U and V)
  VSM.MVector s Word8 -> -- aboveNz (mbCols*2, read top row, write bottom row)
  Int -> -- mbX
  Int ->
  Int -> -- leftNz row 0, row 1
  Maybe (CoeffStats s) -> -- Optional coefficient statistics
  ST s (BoolEncoder, Int, Int)
encodeChromaBlocks chromaOrig chromaRecon stride x y predMode dequantFactors lambda coeffProbs enc coeffBlockType aboveNz mbX leftNz0 leftNz1 mStats = do
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
  let processBlock !blockIdx !e
        | blockIdx >= 4 = return e
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

            -- Trellis-quantize (always use type 2 = UV quant for both U and V)
            _ <- trellisQuantizeBlock dequantFactors 2 residuals coeffProbs ctx 0 lambda

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
            processBlock (blockIdx + 1) e'

  enc' <- processBlock 0 enc

  -- Update aboveNz with bottom row NZ (blocks 2 and 3)
  nz2 <- VSM.read nzGrid 2
  nz3 <- VSM.read nzGrid 3
  VSM.write aboveNz (mbX * 2) nz2
  VSM.write aboveNz (mbX * 2 + 1) nz3

  -- Return right column NZ (blocks 1 and 3) for next MB's left
  newLeft0 <- fromIntegral <$> VSM.read nzGrid 1
  newLeft1 <- fromIntegral <$> VSM.read nzGrid 3

  return (enc', newLeft0, newLeft1)

-- ---------------------------------------------------------------------------
-- Adaptive QP segmentation
-- ---------------------------------------------------------------------------

-- | Compute Y-plane variance for a single block
{-# INLINE computeBlockVariance #-}
computeBlockVariance :: VSM.MVector s Word8 -> Int -> Int -> Int -> ST s Int
computeBlockVariance buf stride bx by = do
  let n = 256 :: Int -- 16x16
      go !i !sumVal !sumSq
        | i >= n =
            -- var = (n * sumSq - sumVal^2) / n^2
            return $ (n * sumSq - sumVal * sumVal) `div` (n * n)
        | otherwise = do
            let !row = i `shiftR` 4
                !col = i .&. 15
                !idx = (by + row) * stride + (bx + col)
            !px <- fromIntegral <$> VSM.unsafeRead buf idx
            go (i + 1) (sumVal + px) (sumSq + px * px)
  go 0 0 0

-- | Compute Y-plane variance for all macroblocks
computeMBVariances ::
  VSM.MVector s Word8 -> -- Y buffer
  Int -> -- Stride (padded width)
  Int -> -- MB rows
  Int -> -- MB cols
  ST s (VU.Vector Int)
computeMBVariances yBuf stride mbRows mbCols = do
  let !n = mbRows * mbCols
  result <- VUM.new n
  let go !i
        | i >= n = VU.unsafeFreeze result
        | otherwise = do
            let !r = i `div` mbCols
                !c = i - r * mbCols
            !var <- computeBlockVariance yBuf stride (c * 16) (r * 16)
            VUM.unsafeWrite result i var
            go (i + 1)
  go 0

-- | Classify macroblocks into 4 segments by variance quartiles.
-- Returns (segment map, count per segment).
classifySegments :: VU.Vector Int -> (VU.Vector Word8, Int, Int, Int, Int)
classifySegments variances =
  let n = VU.length variances
      sorted = VU.fromList $ sort (VU.toList variances)
      -- Quartile boundaries (use floor indices)
      !q1 = sorted VU.! max 0 (n `div` 4 - 1)
      !q2 = sorted VU.! max 0 (n `div` 2 - 1)
      !q3 = sorted VU.! max 0 (3 * n `div` 4 - 1)
      classify v
        | v <= q1 = 0
        | v <= q2 = 1
        | v <= q3 = 2
        | otherwise = 3
      segMap = VU.map classify variances
      !c0 = VU.foldl' (\acc s -> if s == 0 then acc + 1 else acc) 0 segMap
      !c1 = VU.foldl' (\acc s -> if s == 1 then acc + 1 else acc) 0 segMap
      !c2 = VU.foldl' (\acc s -> if s == 2 then acc + 1 else acc) 0 segMap
      !c3 = n - c0 - c1 - c2
   in (segMap, c0, c1, c2, c3)

-- | Compute QP deltas for 4 segments based on base quantizer index.
-- Smooth regions (low variance) get finer quantization (negative delta),
-- busy regions (high variance) get coarser quantization (positive delta).
computeSegmentDeltas :: Int -> VU.Vector Int
computeSegmentDeltas qi
  | qi < 8 = VU.fromList [0, 0, 0, 0] -- No benefit at very high quality
  | otherwise =
      let -- Scale deltas proportionally with qi
          !d0 = negate $ qi * 15 `div` 100 -- smoothest: up to ~15% finer
          !d1 = negate $ qi * 5 `div` 100 -- medium-smooth: up to ~5% finer
          !d2 = 0 -- base
          !d3 = qi * 10 `div` 100 -- busiest: up to ~10% coarser
          -- Clamp so effective qi stays in [0, 127]
          !d0' = max (negate qi) d0
          !d3' = min (127 - qi) d3
       in VU.fromList [d0', d1, d2, d3']

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
