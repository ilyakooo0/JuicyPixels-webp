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
import Codec.Picture.WebP.Internal.VP8.LoopFilter (applyLoopFilter, applySimpleLoopFilterRow)
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
import Data.Word

-- | Decode a VP8 lossy WebP image
decodeVP8 :: B.ByteString -> Either String (Image PixelRGB8)
decodeVP8 bs = do
  header <- parseVP8Header bs

  let width = vp8Width header
      height = vp8Height header
      mbWidth = (width + 15) `div` 16
      mbHeight = (height + 15) `div` 16

  -- Initialize the DCT partition decoder
  -- With 1 DCT partition, the data follows immediately after partition 0
  let dctPartitions = vp8DCTPartitions header
      dctDecoder = case dctPartitions of
        (p : _) -> initBoolDecoder p
        [] -> initBoolDecoder B.empty -- Fallback: empty partition
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
            dequantFact = computeDequantFactors (vp8QuantIndices header) (vp8Segments header) V.! 0

        let !filterLevel = vp8FilterLevel header
            !filterType = vp8FilterType header

        -- Decode all macroblocks with separate decoders for modes and coefficients
        -- NZ state is threaded: left NZ resets to 0 at each row start
        let decodeMacroblocks !mbY !mbX !modeDec !coeffDec !lY0 !lY1 !lY2 !lY3 !lU0 !lU1 !lV0 !lV1 !lDC
              | mbY >= mbHeight = return (modeDec, coeffDec)
              | mbX >= mbWidth = do
                  -- Apply per-row simple loop filter to completed row
                  when (filterLevel > 0 && filterType == 1) $
                    applySimpleLoopFilterRow yBuf (mbWidth * 16) mbY mbWidth filterLevel
                  decodeMacroblocks (mbY + 1) 0 modeDec coeffDec 0 0 0 0 0 0 0 0 0
              | otherwise = do
                  -- Read Y mode from partition 0
                  let (yMode, modeDec1) = boolReadTree kfYModeTree kfYModeProbs modeDec

                  -- Read UV mode from partition 0
                  let (uvMode, modeDec2) = boolReadTree kfUVModeTree kfUVModeProbs modeDec1

                  -- Process macroblock, threading both decoders and NZ state
                  (modeDecAfterMB, coeffDecAfterMB, lY0', lY1', lY2', lY3', lU0', lU1', lV0', lV1', lDC') <-
                    if yMode == 4
                      then do
                        -- B_PRED: read sub-block modes from partition 0, coefficients from DCT partition
                        (modeDec3, coeffDec', bpLY0, bpLY1, bpLY2, bpLY3) <-
                          reconstructBPred
                            yBuf
                            mbY
                            mbX
                            mbWidth
                            modeDec2
                            coeffDec
                            coeffProbs
                            header
                            aboveNzY
                            lY0
                            lY1
                            lY2
                            lY3

                        -- B_PRED has no Y2 block, DC NZ = 0
                        VSM.write aboveNzDC mbX 0

                        -- Reconstruct U and V chroma blocks
                        (coeffDecU, cLU0, cLU1) <-
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
                        (coeffDecV, cLV0, cLV1) <-
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
                        return (modeDec3, coeffDecV, bpLY0, bpLY1, bpLY2, bpLY3, cLU0, cLU1, cLV0, cLV1, 0)
                      else do
                        -- Non-B_PRED: skip flag from partition 0, coefficients from DCT partition
                        let (skip, modeDec3) =
                              if vp8SkipEnabled header
                                then boolRead (vp8ProbSkipFalse header) modeDec2
                                else (False, modeDec2)

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
                            return (modeDec3, coeffDec, 0, 0, 0, 0, 0, 0, 0, 0, 0)
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
                            (coeffDec2, rLY0, rLY1, rLY2, rLY3) <-
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
                            (coeffDec3, rLU0, rLU1) <-
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
                            (coeffDec4, rLV0, rLV1) <-
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
                            return (modeDec3, coeffDec4, rLY0, rLY1, rLY2, rLY3, rLU0, rLU1, rLV0, rLV1, newLDC)

                  -- Continue to next macroblock with updated decoders and NZ state
                  decodeMacroblocks mbY (mbX + 1) modeDecAfterMB coeffDecAfterMB lY0' lY1' lY2' lY3' lU0' lU1' lV0' lV1' lDC'

        (_finalModeDec, _finalCoeffDec) <- decodeMacroblocks 0 0 modeDecoder dctDecoder 0 0 0 0 0 0 0 0 0

        -- Apply loop filter to reconstructed frame
        -- Simple filter (type 1) was already applied per-row above
        -- Normal filter (type 0) is applied post-frame
        when (vp8FilterLevel header > 0 && vp8FilterType header /= 1) $ do
          applyLoopFilter header yBuf (mbWidth * 16) (mbHeight * 16)

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

                -- YUV to RGB conversion (BT.601) - use shiftR 8 instead of div 256
                !r = clamp (yVal + ((360 * (vVal - 128)) `shiftR` 8))
                !g = clamp (yVal - ((88 * (uVal - 128) + 184 * (vVal - 128)) `shiftR` 8))
                !b = clamp (yVal + ((455 * (uVal - 128)) `shiftR` 8))

                rgbIdx = (y * width + x) * 3

            VSM.write rgbBuf rgbIdx (fromIntegral r)
            VSM.write rgbBuf (rgbIdx + 1) (fromIntegral g)
            VSM.write rgbBuf (rgbIdx + 2) (fromIntegral b)

        VS.freeze rgbBuf

  return $ Image width height pixelData

-- | Reconstruct B_PRED macroblock (16 individual 4x4 blocks) with NZ context tracking
-- Sub-block modes are read from modeDecoder (partition 0)
-- Coefficients are read from coeffDecoder (DCT partition)
reconstructBPred ::
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  BoolDecoder -> -- Mode decoder (partition 0) - for sub-block modes
  BoolDecoder -> -- Coefficient decoder (DCT partition)
  VU.Vector Word8 ->
  VP8FrameHeader ->
  VSM.MVector s Word8 -> -- aboveNzY (mbCols*4)
  Int ->
  Int ->
  Int ->
  Int -> -- leftNzY[0..3]
  ST s (BoolDecoder, BoolDecoder, Int, Int, Int, Int)
reconstructBPred yBuf mbY mbX mbStride modeDecoder coeffDecoder coeffProbs header aboveNzY leftNzY0 leftNzY1 leftNzY2 leftNzY3 = do
  let mbYBase = mbY * 16
      mbXBase = mbX * 16
      dequantFact = computeDequantFactors (vp8QuantIndices header) (vp8Segments header) V.! 0

  -- NZ tracking grid for 16 sub-blocks
  nzGrid <- VSM.replicate 16 (0 :: Word8)

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

        -- Read 4x4 intra mode from partition 0
        let probOffset = 0 * 10 * 9 + 0 * 9 -- above=0, left=0
            probs = V.convert $ VU.drop probOffset kfBmodeProbs
            (bMode, modeDec') = boolReadTree kfBmodeTree probs modeDec

        -- Apply 4x4 prediction
        predict4x4 bMode yBuf (mbStride * 16) blockX blockY

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

        return (modeDec', coeffDec')

  -- Decode all 16 4x4 blocks
  let loopBBlocks blockIdx modeDec coeffDec
        | blockIdx >= 16 = return (modeDec, coeffDec)
        | otherwise = do
            (modeDec', coeffDec') <- decodeBBlock blockIdx modeDec coeffDec
            loopBBlocks (blockIdx + 1) modeDec' coeffDec'

  (finalModeDec, finalCoeffDec) <- loopBBlocks 0 modeDecoder coeffDecoder

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

  return (finalModeDec, finalCoeffDec, newLeftY0, newLeftY1, newLeftY2, newLeftY3)

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
  ST s (BoolDecoder, Int, Int, Int, Int)
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

        return dec'

  -- Decode all 16 Y blocks sequentially
  let loopYBlocks blockIdx dec
        | blockIdx >= 16 = return dec
        | otherwise = do
            dec' <- decodeYBlock blockIdx dec
            loopYBlocks (blockIdx + 1) dec'

  finalDec <- loopYBlocks 0 decoder

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

  return (finalDec, newLeftY0, newLeftY1, newLeftY2, newLeftY3)

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
  ST s (BoolDecoder, Int, Int)
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

        return dec'

  -- Decode all 4 UV blocks
  let loopUVBlocks blockIdx dec
        | blockIdx >= 4 = return dec
        | otherwise = do
            dec' <- decodeUVBlock blockIdx dec
            loopUVBlocks (blockIdx + 1) dec'

  finalDec <- loopUVBlocks 0 decoder

  -- Update aboveNz with bottom row NZ (blocks 2 and 3)
  nz2 <- VSM.read nzGrid 2
  nz3 <- VSM.read nzGrid 3
  VSM.write aboveNz (mbX * 2) nz2
  VSM.write aboveNz (mbX * 2 + 1) nz3

  -- Return right column NZ (blocks 1 and 3) for next MB's left
  newLeft0 <- fromIntegral <$> VSM.read nzGrid 1
  newLeft1 <- fromIntegral <$> VSM.read nzGrid 3

  return (finalDec, newLeft0, newLeft1)

-- | Clamp value to 0-255 range
clamp :: Int -> Int
clamp x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = x

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
