{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8
  ( decodeVP8,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.VP8.BoolDecoder
import Codec.Picture.WebP.Internal.VP8.Coefficients
import Codec.Picture.WebP.Internal.VP8.Dequant
import Codec.Picture.WebP.Internal.VP8.Header
import Codec.Picture.WebP.Internal.VP8.IDCT
import Codec.Picture.WebP.Internal.VP8.LoopFilter (applyLoopFilterFrame)
import Codec.Picture.WebP.Internal.VP8.Predict
import Codec.Picture.WebP.Internal.VP8.Tables
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import Data.Int (Int16, Int8)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- | Decode a VP8 lossy WebP image
decodeVP8 :: B.ByteString -> Either String (Image PixelRGB8)
decodeVP8 bs = do
  header <- parseVP8Header bs

  let width = vp8Width header
      height = vp8Height header
      mbWidth = (width + 15) `div` 16
      mbHeight = (height + 15) `div` 16

  -- Initialize one decoder per DCT partition
  -- MB row r reads its coefficients from partition (r mod N)
  let dctPartitions = vp8DCTPartitions header
      dctDecoders = case dctPartitions of
        [] -> V.singleton (initBoolDecoder B.empty) -- Fallback: empty partition
        ps -> V.fromList (map initBoolDecoder ps)
      numDctPartitions = V.length dctDecoders
  let pixelData = runST $ do
        -- Allocate YUV buffers
        yBuf <- VSM.replicate (mbWidth * 16 * mbHeight * 16) (128 :: Word8)
        uBuf <- VSM.replicate (mbWidth * 8 * mbHeight * 8) (128 :: Word8)
        vBuf <- VSM.replicate (mbWidth * 8 * mbHeight * 8) (128 :: Word8)

        -- Allocate NZ tracking arrays (persist across MB rows)
        aboveNzY <- VSM.replicate (mbWidth * 4) (0 :: Word8)
        aboveNzU <- VSM.replicate (mbWidth * 2) (0 :: Word8)
        aboveNzV <- VSM.replicate (mbWidth * 2) (0 :: Word8)
        aboveNzDC <- VSM.replicate mbWidth (0 :: Word8)

        -- Partition 0 decoder: reads modes (positioned after compressed header)
        let modeDecoder = vp8Decoder header
            coeffProbs = vp8CoeffProbs header
            dequantFactorsVec = computeDequantFactors (vp8QuantIndices header) (vp8Segments header)
            -- Segmentation: do we need to read per-MB segment IDs?
            segReadInfo = case vp8Segments header of
              Just si | segmentEnabled si && segmentUpdateMap si -> Just (segmentTreeProbs si)
              _ -> Nothing

        let !filterLevel = vp8FilterLevel header
            !filterType = vp8FilterType header

        -- B_PRED sub-block mode context (4 modes per MB column, bottom row)
        aboveBModes <- VSM.replicate (mbWidth * 4) (0 :: Word8)

        -- Per-MB loop filter info, recorded during decode and consumed by the
        -- whole-frame loop filter pass after reconstruction completes
        mbFilterLevels <- VUM.replicate (mbWidth * mbHeight) (0 :: Int)
        mbFilterInner <- VUM.replicate (mbWidth * mbHeight) False

        -- Decode all macroblocks with separate decoders for modes and coefficients
        -- NZ state is threaded: left NZ resets to 0 at each row start
        let decodeMacroblocks !mbY !mbX !modeDec !coeffDecs !lY0 !lY1 !lY2 !lY3 !lU0 !lU1 !lV0 !lV1 !lDC !lBM0 !lBM1 !lBM2 !lBM3
              | mbY >= mbHeight = return (modeDec, coeffDecs)
              | mbX >= mbWidth =
                  -- NOTE: the loop filter runs as a separate whole-frame pass
                  -- after all MBs are reconstructed, so intra prediction always
                  -- reads UNFILTERED pixels (RFC 6386 reference decoder behavior)
                  decodeMacroblocks (mbY + 1) 0 modeDec coeffDecs 0 0 0 0 0 0 0 0 0 0 0 0 0
              | otherwise = do
                  -- Read segment ID from partition 0 (if segmentation enabled)
                  let (!segId, !modeDec0) = case segReadInfo of
                        Nothing -> (0, modeDec)
                        Just (sp0, sp1, sp2) -> decodeSegmentId sp0 sp1 sp2 modeDec
                      !dequantFact = dequantFactorsVec V.! segId

                  -- RFC 6386 §19.3: mb_skip_coeff follows segment_id and precedes Y mode
                  let (!skip, !modeDecS) =
                        if vp8SkipEnabled header
                          then boolRead (vp8ProbSkipFalse header) modeDec0
                          else (False, modeDec0)

                  -- Read Y mode from partition 0
                  let (yMode, modeDec1) = boolReadTree kfYModeTree kfYModeProbs modeDecS

                  -- MB row uses DCT partition (row mod N)
                  let !partIdx = mbY `mod` numDctPartitions
                      !coeffDec = coeffDecs V.! partIdx

                  -- Process macroblock, threading both decoders and NZ state
                  -- RFC 6386 §11.5: Y mode → (if B_PRED: sub-block modes) → UV mode
                  (modeDecAfterMB, coeffDecAfterMB, lY0', lY1', lY2', lY3', lU0', lU1', lV0', lV1', lDC', lBM0', lBM1', lBM2', lBM3', hasCoeffs) <-
                    if yMode == 4
                      then do
                        -- Read above B modes for context
                        aBM0 <- VSM.read aboveBModes (mbX * 4)
                        aBM1 <- VSM.read aboveBModes (mbX * 4 + 1)
                        aBM2 <- VSM.read aboveBModes (mbX * 4 + 2)
                        aBM3 <- VSM.read aboveBModes (mbX * 4 + 3)

                        -- B_PRED: read sub-block modes from partition 0 BEFORE UV mode
                        (modeDec2, coeffDec', bpLY0, bpLY1, bpLY2, bpLY3, bpAnyNz, bpModes) <-
                          reconstructBPred
                            yBuf
                            mbY
                            mbX
                            mbWidth
                            skip
                            modeDec1
                            coeffDec
                            coeffProbs
                            dequantFact
                            aboveNzY
                            lY0
                            lY1
                            lY2
                            lY3
                            aBM0
                            aBM1
                            aBM2
                            aBM3
                            lBM0
                            lBM1
                            lBM2
                            lBM3

                        -- Read UV mode AFTER sub-block modes (RFC 6386 §11.5)
                        let (uvMode, modeDec3) = boolReadTree kfUVModeTree kfUVModeProbs modeDec2

                        -- B_PRED has no Y2 block: the Y2/DC NZ context passes through unchanged

                        -- Update above B modes with bottom row
                        bm12 <- VSM.read bpModes 12
                        bm13 <- VSM.read bpModes 13
                        bm14 <- VSM.read bpModes 14
                        bm15 <- VSM.read bpModes 15
                        VSM.write aboveBModes (mbX * 4) bm12
                        VSM.write aboveBModes (mbX * 4 + 1) bm13
                        VSM.write aboveBModes (mbX * 4 + 2) bm14
                        VSM.write aboveBModes (mbX * 4 + 3) bm15

                        -- Reconstruct U and V chroma blocks
                        (coeffDecV, cLU0, cLU1, cLV0, cLV1, cAnyNz) <-
                          if skip
                            then do
                              -- Skipped MB: chroma is pure prediction, contexts reset to 0
                              predict8x8 uvMode uBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                              predict8x8 uvMode vBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                              forM_ [0 .. 1] $ \i -> VSM.write aboveNzU (mbX * 2 + i) 0
                              forM_ [0 .. 1] $ \i -> VSM.write aboveNzV (mbX * 2 + i) 0
                              return (coeffDec', 0, 0, 0, 0, False)
                            else do
                              (coeffDecU, cLU0, cLU1, uAnyNz) <-
                                reconstructChroma
                                  uBuf
                                  mbY
                                  mbX
                                  mbWidth
                                  uvMode
                                  coeffDec'
                                  coeffProbs
                                  dequantFact
                                  2
                                  aboveNzU
                                  lU0
                                  lU1
                              (coeffDecV, cLV0, cLV1, vAnyNz) <-
                                reconstructChroma
                                  vBuf
                                  mbY
                                  mbX
                                  mbWidth
                                  uvMode
                                  coeffDecU
                                  coeffProbs
                                  dequantFact
                                  2
                                  aboveNzV
                                  lV0
                                  lV1
                              return (coeffDecV, cLU0, cLU1, cLV0, cLV1, uAnyNz || vAnyNz)
                        -- Right column B modes for next MB's left context
                        bm3 <- VSM.read bpModes 3
                        bm7 <- VSM.read bpModes 7
                        bm11 <- VSM.read bpModes 11
                        bm15' <- VSM.read bpModes 15
                        let !newLBM0 = fromIntegral bm3
                            !newLBM1 = fromIntegral bm7
                            !newLBM2 = fromIntegral bm11
                            !newLBM3 = fromIntegral bm15'
                        return (modeDec3, coeffDecV, bpLY0, bpLY1, bpLY2, bpLY3, cLU0, cLU1, cLV0, cLV1, lDC, newLBM0, newLBM1, newLBM2, newLBM3, bpAnyNz || cAnyNz)
                      else do
                        -- Non-B_PRED: read UV mode (skip flag was read before the Y mode)
                        let (uvMode, modeDec2) = boolReadTree kfUVModeTree kfUVModeProbs modeDec1
                            !yBM = yModeToBMode yMode

                        -- Non-B_PRED MB contributes its Y mode mapped to a B mode
                        -- as context for adjacent B_PRED MBs
                        forM_ [0 .. 3] $ \i -> VSM.write aboveBModes (mbX * 4 + i) (fromIntegral yBM)

                        if skip
                          then do
                            -- All coefficients are zero, just use prediction
                            predict16x16 yMode yBuf (mbWidth * 16) (mbX * 16) (mbY * 16)
                            predict8x8 uvMode uBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                            predict8x8 uvMode vBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                            -- Zero out above NZ for this MB
                            forM_ [0 .. 3] $ \i -> VSM.write aboveNzY (mbX * 4 + i) 0
                            forM_ [0 .. 1] $ \i -> VSM.write aboveNzU (mbX * 2 + i) 0
                            forM_ [0 .. 1] $ \i -> VSM.write aboveNzV (mbX * 2 + i) 0
                            VSM.write aboveNzDC mbX 0
                            return (modeDec2, coeffDec, 0, 0, 0, 0, 0, 0, 0, 0, 0, yBM, yBM, yBM, yBM, False)
                          else do
                            -- Decode Y2 block with NZ context
                            aNzDC <- VSM.read aboveNzDC mbX
                            let !dcCtx = min 2 (fromIntegral aNzDC + lDC)
                            (y2Coeffs, y2nz, coeffDec1) <- decodeCoefficients coeffDec coeffProbs 1 dcCtx 0

                            -- Dequantize and apply WHT
                            dequantizeBlock dequantFact 1 y2Coeffs
                            iwht4x4 y2Coeffs

                            VSM.write aboveNzDC mbX (if y2nz then 1 else 0)
                            let !newLDC = if y2nz then 1 else 0

                            -- Decode and reconstruct 16 Y blocks with NZ context
                            (coeffDec2, rLY0, rLY1, rLY2, rLY3, yAnyNz) <-
                              reconstructMB16x16
                                yBuf
                                mbY
                                mbX
                                mbWidth
                                yMode
                                y2Coeffs
                                coeffDec1
                                coeffProbs
                                dequantFact
                                aboveNzY
                                lY0
                                lY1
                                lY2
                                lY3

                            -- Reconstruct U and V blocks with NZ context
                            (coeffDec3, rLU0, rLU1, uAnyNz) <-
                              reconstructChroma
                                uBuf
                                mbY
                                mbX
                                mbWidth
                                uvMode
                                coeffDec2
                                coeffProbs
                                dequantFact
                                2
                                aboveNzU
                                lU0
                                lU1
                            (coeffDec4, rLV0, rLV1, vAnyNz) <-
                              reconstructChroma
                                vBuf
                                mbY
                                mbX
                                mbWidth
                                uvMode
                                coeffDec3
                                coeffProbs
                                dequantFact
                                2
                                aboveNzV
                                lV0
                                lV1
                            return (modeDec2, coeffDec4, rLY0, rLY1, rLY2, rLY3, rLU0, rLU1, rLV0, rLV1, newLDC, yBM, yBM, yBM, yBM, y2nz || yAnyNz || uAnyNz || vAnyNz)

                  -- Record per-MB loop filter info (RFC 6386 §15.2):
                  -- level from segment strength + (keyframe) intra/mode deltas;
                  -- interior edges filtered only if the MB has nonzero
                  -- coefficients or uses B_PRED (libwebp: f_inner_ = is_i4x4 || !skip)
                  let !isBPred = yMode == 4
                      !mbIdx = mbY * mbWidth + mbX
                  VUM.write mbFilterLevels mbIdx (computeMBFilterLevel header segId isBPred)
                  VUM.write mbFilterInner mbIdx (isBPred || hasCoeffs)

                  -- Continue to next macroblock with updated decoders and NZ state
                  let !coeffDecs' = coeffDecs V.// [(partIdx, coeffDecAfterMB)]
                  decodeMacroblocks mbY (mbX + 1) modeDecAfterMB coeffDecs' lY0' lY1' lY2' lY3' lU0' lU1' lV0' lV1' lDC' lBM0' lBM1' lBM2' lBM3'

        (_finalModeDec, _finalCoeffDecs) <- decodeMacroblocks 0 0 modeDecoder dctDecoders 0 0 0 0 0 0 0 0 0 0 0 0 0

        -- Whole-frame loop filter pass over the fully reconstructed frame.
        -- A frame-level filter level of 0 disables filtering entirely
        -- (libwebp: filter_type_ = (hdr->level_ == 0) ? 0 : ...).
        when (filterLevel > 0) $ do
          levels <- VU.freeze mbFilterLevels
          inners <- VU.freeze mbFilterInner
          applyLoopFilterFrame
            filterType
            (vp8Sharpness header)
            levels
            inners
            yBuf
            (mbWidth * 16)
            uBuf
            (mbWidth * 8)
            vBuf
            (mbWidth * 8)
            mbHeight
            mbWidth

        -- Convert YUV to RGB
        yData <- VS.freeze yBuf
        uData <- VS.freeze uBuf
        vData <- VS.freeze vBuf

        rgbBuf <- VSM.new (width * height * 3)

        forM_ [0 .. height - 1] $ \y ->
          forM_ [0 .. width - 1] $ \x -> do
            let !yIdx = y * mbWidth * 16 + x
                !chromaY = y `shiftR` 1
                !chromaX = x `shiftR` 1
                !uIdx = chromaY * mbWidth * 8 + chromaX
                !vIdx = uIdx

                !yVal = fromIntegral (yData `VS.unsafeIndex` yIdx) :: Int
                !uVal = fromIntegral (uData `VS.unsafeIndex` uIdx) :: Int
                !vVal = fromIntegral (vData `VS.unsafeIndex` vIdx) :: Int

                -- YUV to RGB conversion (BT.601 studio swing, libwebp/libvpx)
                !c = 298 * (yVal - 16)
                !r = clamp ((c + 409 * (vVal - 128) + 128) `shiftR` 8)
                !g = clamp ((c - 100 * (uVal - 128) - 208 * (vVal - 128) + 128) `shiftR` 8)
                !b = clamp ((c + 516 * (uVal - 128) + 128) `shiftR` 8)

                rgbIdx = (y * width + x) * 3

            VSM.write rgbBuf rgbIdx (fromIntegral r)
            VSM.write rgbBuf (rgbIdx + 1) (fromIntegral g)
            VSM.write rgbBuf (rgbIdx + 2) (fromIntegral b)

        VS.freeze rgbBuf

  return $ Image width height pixelData

-- | Reconstruct B_PRED macroblock (16 individual 4x4 blocks) with NZ context tracking
-- Sub-block modes are read from modeDecoder (partition 0) using kfBmodeProbs[above][left]
-- Coefficients are read from coeffDecoder (DCT partition)
-- When skipCoeff is set, no coefficient data is read (residual is zero)
-- Returns: (modeDec, coeffDec, leftNzY0..3, anyNz, modeGrid) where modeGrid has 16 decoded modes
reconstructBPred ::
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  Bool -> -- mb_skip_coeff: no coefficient data for this MB
  BoolDecoder -> -- Mode decoder (partition 0) - for sub-block modes
  BoolDecoder -> -- Coefficient decoder (DCT partition)
  VU.Vector Word8 ->
  DequantFactors ->
  VSM.MVector s Word8 -> -- aboveNzY (mbCols*4)
  Int ->
  Int ->
  Int ->
  Int -> -- leftNzY[0..3]
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 -> -- aboveBModes (from above MB's bottom row)
  Int ->
  Int ->
  Int ->
  Int -> -- leftBModes (from left MB's right column)
  ST s (BoolDecoder, BoolDecoder, Int, Int, Int, Int, Bool, VSM.MVector s Word8)
reconstructBPred yBuf mbY mbX mbStride skipCoeff modeDecoder coeffDecoder coeffProbs dequantFact aboveNzY leftNzY0 leftNzY1 leftNzY2 leftNzY3 aBM0 aBM1 aBM2 aBM3 lBM0 lBM1 lBM2 lBM3 = do
  let mbYBase = mbY * 16
      mbXBase = mbX * 16

  -- NZ tracking grid for 16 sub-blocks
  nzGrid <- VSM.replicate 16 (0 :: Word8)
  -- Mode grid for 16 sub-blocks (for context and return)
  modeGrid <- VSM.replicate 16 (0 :: Word8)

  -- Read above NZ for this MB's Y columns
  aNzCol0 <- VSM.read aboveNzY (mbX * 4)
  aNzCol1 <- VSM.read aboveNzY (mbX * 4 + 1)
  aNzCol2 <- VSM.read aboveNzY (mbX * 4 + 2)
  aNzCol3 <- VSM.read aboveNzY (mbX * 4 + 3)

  -- Decode each 4x4 block with its own mode and NZ context
  let decodeBBlock blockIdx modeDec coeffDec = do
        let !row = blockIdx `shiftR` 2 -- div 4
            !col = blockIdx .&. 3 -- mod 4
            blockY = mbYBase + row * 4
            blockX = mbXBase + col * 4

        -- Get above/left B mode context for probability lookup
        aboveBMode <-
          if row == 0
            then return $ fromIntegral $ case col of
              0 -> aBM0
              1 -> aBM1
              2 -> aBM2
              _ -> aBM3
            else fromIntegral <$> VSM.read modeGrid ((row - 1) * 4 + col)
        leftBMode <-
          if col == 0
            then return $ case row of
              0 -> lBM0
              1 -> lBM1
              2 -> lBM2
              _ -> lBM3
            else fromIntegral <$> VSM.read modeGrid (row * 4 + col - 1)

        -- Read 4x4 intra mode from partition 0 with proper context
        let !probOffset = aboveBMode * 90 + leftBMode * 9
            probs = V.convert $ VU.slice probOffset 9 kfBmodeProbs
            (bMode, modeDec') = boolReadTree kfBmodeTree probs modeDec

        -- Store decoded mode for context
        VSM.write modeGrid blockIdx (fromIntegral bMode)

        -- Apply 4x4 prediction
        predict4x4 bMode yBuf (mbStride * 16) blockX blockY

        if skipCoeff
          then return (modeDec', coeffDec, False) -- No coefficient data: residual is zero
          else do
            -- Compute NZ context
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

            -- Decode coefficients from DCT partition with NZ context
            -- blockType=3 for i4-AC (B_PRED Y blocks with DC)
            (coeffs, hasNonzero, coeffDec') <- decodeCoefficients coeffDec coeffProbs 3 ctx 0

            -- Track NZ
            VSM.write nzGrid blockIdx (if hasNonzero then 1 else 0)

            -- Dequantize
            dequantizeBlock dequantFact 3 coeffs -- Type 3: Y block with DC

            -- Apply IDCT
            idct4x4 coeffs

            -- Add to prediction and clamp
            forM_ [0 :: Int .. 3] $ \dy ->
              forM_ [0 :: Int .. 3] $ \dx -> do
                let yIdx = (blockY + dy) * mbStride * 16 + (blockX + dx)
                pred <- VSM.read yBuf yIdx
                residual <- VSM.read coeffs (dy * 4 + dx)
                let reconstructed = fromIntegral pred + fromIntegral residual
                    clamped = fromIntegral $ min 255 $ max 0 reconstructed
                VSM.write yBuf yIdx clamped

            return (modeDec', coeffDec', hasNonzero)

  -- Decode all 16 4x4 blocks
  let loopBBlocks blockIdx modeDec coeffDec !anyNz
        | blockIdx >= 16 = return (modeDec, coeffDec, anyNz)
        | otherwise = do
            (modeDec', coeffDec', nz) <- decodeBBlock blockIdx modeDec coeffDec
            loopBBlocks (blockIdx + 1) modeDec' coeffDec' (anyNz || nz)

  (finalModeDec, finalCoeffDec, anyNz) <- loopBBlocks 0 modeDecoder coeffDecoder False

  -- Update aboveNzY with bottom row NZ (blocks 12, 13, 14, 15)
  nz12 <- VSM.read nzGrid 12
  nz13 <- VSM.read nzGrid 13
  nz14 <- VSM.read nzGrid 14
  nz15 <- VSM.read nzGrid 15
  VSM.write aboveNzY (mbX * 4) nz12
  VSM.write aboveNzY (mbX * 4 + 1) nz13
  VSM.write aboveNzY (mbX * 4 + 2) nz14
  VSM.write aboveNzY (mbX * 4 + 3) nz15

  -- Return right column NZ (blocks 3, 7, 11, 15) and the mode grid
  newLeftY0 <- fromIntegral <$> VSM.read nzGrid 3
  newLeftY1 <- fromIntegral <$> VSM.read nzGrid 7
  newLeftY2 <- fromIntegral <$> VSM.read nzGrid 11
  newLeftY3 <- fromIntegral <$> VSM.read nzGrid 15

  return (finalModeDec, finalCoeffDec, newLeftY0, newLeftY1, newLeftY2, newLeftY3, anyNz, modeGrid)

-- | Reconstruct 16x16 macroblock from coefficients (DCT partition) with NZ context tracking
reconstructMB16x16 ::
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  Int ->
  VSM.MVector s Int16 ->
  BoolDecoder -> -- Coefficient decoder (DCT partition)
  VU.Vector Word8 ->
  DequantFactors ->
  VSM.MVector s Word8 -> -- aboveNzY (mbCols*4)
  Int ->
  Int ->
  Int ->
  Int -> -- leftNzY[0..3]
  ST s (BoolDecoder, Int, Int, Int, Int, Bool)
reconstructMB16x16 yBuf mbY mbX mbStride yMode y2Coeffs decoder coeffProbs dequantFact aboveNzY leftNzY0 leftNzY1 leftNzY2 leftNzY3 = do
  let mbYBase = mbY * 16
      mbXBase = mbX * 16

  -- First apply prediction for the whole 16x16 block
  predict16x16 yMode yBuf (mbStride * 16) mbXBase mbYBase

  -- NZ tracking grid for 16 sub-blocks
  nzGrid <- VSM.replicate 16 (0 :: Word8)

  -- Read above NZ for this MB's Y columns
  aNzCol0 <- VSM.read aboveNzY (mbX * 4)
  aNzCol1 <- VSM.read aboveNzY (mbX * 4 + 1)
  aNzCol2 <- VSM.read aboveNzY (mbX * 4 + 2)
  aNzCol3 <- VSM.read aboveNzY (mbX * 4 + 3)

  -- Decode and apply each 4x4 Y block with NZ context
  let decodeYBlock blockIdx dec = do
        let !row = blockIdx `shiftR` 2 -- div 4
            !col = blockIdx .&. 3 -- mod 4

        -- Compute NZ context
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

        -- Decode coefficients with NZ context
        -- Block type 0 (Y after Y2), start at pos 1
        (coeffs, hasNonzero, dec') <- decodeCoefficients dec coeffProbs 0 ctx 1

        -- Track NZ
        VSM.write nzGrid blockIdx (if hasNonzero then 1 else 0)

        -- Set DC from Y2 block
        y2dc <- VSM.read y2Coeffs blockIdx
        VSM.write coeffs 0 y2dc

        -- Dequantize
        dequantizeBlock dequantFact 0 coeffs

        -- Apply IDCT
        idct4x4 coeffs

        -- Add to prediction and clamp
        forM_ [0 :: Int .. 3] $ \dy ->
          forM_ [0 :: Int .. 3] $ \dx -> do
            let yIdx = (mbYBase + row * 4 + dy) * mbStride * 16 + (mbXBase + col * 4 + dx)
            pred <- VSM.read yBuf yIdx
            residual <- VSM.read coeffs (dy * 4 + dx)
            let reconstructed = fromIntegral pred + fromIntegral residual
                clamped = fromIntegral $ min 255 $ max 0 reconstructed
            VSM.write yBuf yIdx clamped

        return (dec', hasNonzero)

  -- Decode all 16 Y blocks sequentially
  let loopYBlocks blockIdx dec !anyNz
        | blockIdx >= 16 = return (dec, anyNz)
        | otherwise = do
            (dec', nz) <- decodeYBlock blockIdx dec
            loopYBlocks (blockIdx + 1) dec' (anyNz || nz)

  (finalDec, anyNz) <- loopYBlocks 0 decoder False

  -- Update aboveNzY with bottom row NZ (blocks 12, 13, 14, 15)
  nz12 <- VSM.read nzGrid 12
  nz13 <- VSM.read nzGrid 13
  nz14 <- VSM.read nzGrid 14
  nz15 <- VSM.read nzGrid 15
  VSM.write aboveNzY (mbX * 4) nz12
  VSM.write aboveNzY (mbX * 4 + 1) nz13
  VSM.write aboveNzY (mbX * 4 + 2) nz14
  VSM.write aboveNzY (mbX * 4 + 3) nz15

  -- Return right column NZ (blocks 3, 7, 11, 15)
  newLeftY0 <- fromIntegral <$> VSM.read nzGrid 3
  newLeftY1 <- fromIntegral <$> VSM.read nzGrid 7
  newLeftY2 <- fromIntegral <$> VSM.read nzGrid 11
  newLeftY3 <- fromIntegral <$> VSM.read nzGrid 15

  return (finalDec, newLeftY0, newLeftY1, newLeftY2, newLeftY3, anyNz)

-- | Reconstruct chroma blocks (U or V) from DCT partition with NZ context tracking
-- coeffBlockType should be 2 for both U and V per libwebp convention
-- Dequantization always uses type 2 (UV) for both U and V
reconstructChroma ::
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  Int ->
  BoolDecoder -> -- Coefficient decoder (DCT partition)
  VU.Vector Word8 ->
  DequantFactors ->
  Int -> -- Coefficient block type: 2 for both U and V
  VSM.MVector s Word8 -> -- aboveNz (mbCols*2)
  Int ->
  Int -> -- leftNz row 0, row 1
  ST s (BoolDecoder, Int, Int, Bool)
reconstructChroma uvBuf mbY mbX mbStride uvMode decoder coeffProbs dequantFact coeffBlockType aboveNz leftNz0 leftNz1 = do
  let mbUVY = mbY * 8
      mbUVX = mbX * 8

  -- Apply prediction for 8x8 chroma block
  predict8x8 uvMode uvBuf (mbStride * 8) mbUVX mbUVY

  -- NZ tracking grid for 4 blocks (2x2)
  nzGrid <- VSM.replicate 4 (0 :: Word8)

  -- Read above NZ
  aNzCol0 <- VSM.read aboveNz (mbX * 2)
  aNzCol1 <- VSM.read aboveNz (mbX * 2 + 1)

  -- Decode and apply each 4x4 chroma block (4 blocks total for 8x8) with NZ context
  let decodeUVBlock blockIdx dec = do
        let !row = blockIdx `shiftR` 1 -- div 2
            !col = blockIdx .&. 1 -- mod 2

        -- Compute NZ context
        aboveNzVal <-
          if row == 0
            then return $ fromIntegral $ if col == 0 then aNzCol0 else aNzCol1
            else fromIntegral <$> VSM.read nzGrid col -- block above: row 0, same col
        leftNzVal <-
          if col == 0
            then return $ if row == 0 then leftNz0 else leftNz1
            else fromIntegral <$> VSM.read nzGrid (row * 2) -- block to the left
        let !ctx = min 2 (aboveNzVal + leftNzVal)

        -- Decode coefficients with NZ context
        (coeffs, hasNonzero, dec') <- decodeCoefficients dec coeffProbs coeffBlockType ctx 0

        -- Track NZ
        VSM.write nzGrid blockIdx (if hasNonzero then 1 else 0)

        -- Dequantize (always use type 2 = UV dequant for both U and V)
        dequantizeBlock dequantFact 2 coeffs

        -- Apply IDCT
        idct4x4 coeffs

        -- Add to prediction and clamp
        forM_ [0 :: Int .. 3] $ \dy ->
          forM_ [0 :: Int .. 3] $ \dx -> do
            let uvIdx = (mbUVY + row * 4 + dy) * mbStride * 8 + (mbUVX + col * 4 + dx)
            pred <- VSM.read uvBuf uvIdx
            residual <- VSM.read coeffs (dy * 4 + dx)
            let reconstructed = fromIntegral pred + fromIntegral residual
                clamped = fromIntegral $ min 255 $ max 0 reconstructed
            VSM.write uvBuf uvIdx clamped

        return (dec', hasNonzero)

  -- Decode all 4 UV blocks
  let loopUVBlocks blockIdx dec !anyNz
        | blockIdx >= 4 = return (dec, anyNz)
        | otherwise = do
            (dec', nz) <- decodeUVBlock blockIdx dec
            loopUVBlocks (blockIdx + 1) dec' (anyNz || nz)

  (finalDec, anyNz) <- loopUVBlocks 0 decoder False

  -- Update aboveNz with bottom row NZ (blocks 2 and 3)
  nz2 <- VSM.read nzGrid 2
  nz3 <- VSM.read nzGrid 3
  VSM.write aboveNz (mbX * 2) nz2
  VSM.write aboveNz (mbX * 2 + 1) nz3

  -- Return right column NZ (blocks 1 and 3) for next MB's left
  newLeft0 <- fromIntegral <$> VSM.read nzGrid 1
  newLeft1 <- fromIntegral <$> VSM.read nzGrid 3

  return (finalDec, newLeft0, newLeft1, anyNz)

-- | Per-MB loop filter level (RFC 6386 §15.2, keyframe):
-- start from the frame filter level; if segmentation provides filter
-- strengths, use them (absolute, or as a delta to the frame level),
-- clamped to [0, 63]; if loop filter deltas are enabled, every keyframe
-- MB is INTRA so add the INTRA ref-frame delta (index 0), plus the
-- B_PRED mode delta (index 0) for B_PRED MBs, clamped to [0, 63].
computeMBFilterLevel :: VP8FrameHeader -> Int -> Bool -> Int
computeMBFilterLevel header segId isBPred = withDeltas
  where
    frameLevel = vp8FilterLevel header
    segLevel = case vp8Segments header of
      Just si
        | segmentEnabled si ->
            let v = segmentFilterStrength si VU.! segId
             in clamp63 $ if segmentAbsoluteMode si then v else frameLevel + v
      _ -> frameLevel
    withDeltas = case vp8FilterDeltas header of
      Just fd ->
        clamp63 $
          segLevel
            + (fdRefLfDelta fd VU.! 0) -- INTRA_FRAME ref delta
            + (if isBPred then fdModeLfDelta fd VU.! 0 else 0) -- B_PRED mode delta
      Nothing -> segLevel
    clamp63 = max 0 . min 63

-- | Clamp value to 0-255 range
clamp :: Int -> Int
clamp x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = x

-- | Map a non-B_PRED keyframe Y mode to the B mode it implies as
-- above/left context for adjacent B_PRED macroblocks (RFC 6386 §11.5)
{-# INLINE yModeToBMode #-}
yModeToBMode :: Int -> Int
yModeToBMode 1 = 2 -- V_PRED → B_VE_PRED
yModeToBMode 2 = 3 -- H_PRED → B_HE_PRED
yModeToBMode 3 = 1 -- TM_PRED → B_TM_PRED
yModeToBMode _ = 0 -- DC_PRED → B_DC_PRED

-- Keyframe Y mode tree (from RFC 6386)
-- Tree structure: indices are pairs (left, right). Negative = leaf returning -value, 0 = leaf returning 0
-- B_PRED=4, DC_PRED=0, V_PRED=1, H_PRED=2, TM_PRED=3
-- Bit patterns: B_PRED="0", DC_PRED="100", V_PRED="101", H_PRED="110", TM_PRED="111"
kfYModeTree :: V.Vector Int8
kfYModeTree =
  V.fromList
    [ -4,
      2, -- B_PRED (4) at code "0", else go to index 2
      4,
      6, -- go to index 4 at code "10", go to index 6 at code "11"
      0,
      -1, -- DC_PRED (0) at code "100", V_PRED (1) at code "101"
      -2,
      -3 -- H_PRED (2) at code "110", TM_PRED (3) at code "111"
    ]

-- Keyframe Y mode probabilities (matching libwebp hardcoded values)
-- boolReadTree uses node-indexed probs[i/2]:
--   node 0 (root): probs[0]=145, node 2: probs[1]=156,
--   node 4 (DC/V): probs[2]=163, node 6 (H/TM): probs[3]=128
kfYModeProbs :: V.Vector Word8
kfYModeProbs = V.fromList [145, 156, 163, 128]

-- Keyframe UV mode tree (from RFC 6386)
-- UV mode doesn't have B_PRED, only DC/V/H/TM (0-3)
-- Bit patterns: DC_PRED="0", V_PRED="10", H_PRED="110", TM_PRED="111"
kfUVModeTree :: V.Vector Int8
kfUVModeTree =
  V.fromList
    [ 0,
      2, -- DC_PRED (0) at code "0", else go to index 2
      -1,
      4, -- V_PRED (1) at code "10", else go to index 4
      -2,
      -3 -- H_PRED (2) at code "110", TM_PRED (3) at code "111"
    ]

-- Keyframe UV mode probabilities (3 probabilities for 3 decision points)
kfUVModeProbs :: V.Vector Word8
kfUVModeProbs = V.fromList [142, 114, 183]

-- | Decode segment ID (0-3) using the VP8 balanced segment tree.
-- Tree (matching libwebp): prob[0] splits {0,1} vs {2,3},
-- prob[1] splits 0 vs 1, prob[2] splits 2 vs 3.
{-# INLINE decodeSegmentId #-}
decodeSegmentId :: Word8 -> Word8 -> Word8 -> BoolDecoder -> (Int, BoolDecoder)
decodeSegmentId p0 p1 p2 dec =
  let (bit0, d1) = boolRead p0 dec
   in if not bit0
        then -- left: segment 0 or 1
          let (bit1, d2) = boolRead p1 d1
           in if not bit1 then (0, d2) else (1, d2)
        else -- right: segment 2 or 3
          let (bit2, d2) = boolRead p2 d1
           in if not bit2 then (2, d2) else (3, d2)
