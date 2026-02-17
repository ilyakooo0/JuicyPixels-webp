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
import Codec.Picture.WebP.Internal.VP8.LoopFilter
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

        -- Partition 0 decoder: reads modes (positioned after compressed header)
        let modeDecoder = vp8Decoder header
            coeffProbs = vp8CoeffProbs header

        -- Decode all macroblocks with separate decoders for modes and coefficients
        let decodeMacroblocks !mbY !mbX !modeDec !coeffDec
              | mbY >= mbHeight = return (modeDec, coeffDec)
              | mbX >= mbWidth = decodeMacroblocks (mbY + 1) 0 modeDec coeffDec
              | otherwise = do
                  -- Read Y mode from partition 0
                  let (yMode, modeDec1) = boolReadTree kfYModeTree kfYModeProbs modeDec

                  -- Read UV mode from partition 0
                  let (uvMode, modeDec2) = boolReadTree kfUVModeTree kfUVModeProbs modeDec1

                  -- Process macroblock, threading both decoders
                  (modeDecAfterMB, coeffDecAfterMB) <-
                    if yMode == 4
                      then do
                        -- B_PRED: read sub-block modes from partition 0, coefficients from DCT partition
                        (modeDec3, coeffDec') <- reconstructBPred yBuf mbY mbX mbWidth modeDec2 coeffDec coeffProbs header

                        -- Reconstruct U and V with 8x8 prediction
                        predict8x8 uvMode uBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                        predict8x8 uvMode vBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                        coeffDecU <- reconstructChroma uBuf mbY mbX mbWidth uvMode coeffDec' coeffProbs (computeDequantFactors (vp8QuantIndices header) (vp8Segments header) V.! 0) 2
                        coeffDecV <- reconstructChroma vBuf mbY mbX mbWidth uvMode coeffDecU coeffProbs (computeDequantFactors (vp8QuantIndices header) (vp8Segments header) V.! 0) 3
                        return (modeDec3, coeffDecV)
                      else do
                        -- Non-B_PRED: skip flag from partition 0, coefficients from DCT partition
                        let (skip, modeDec3) =
                              if vp8SkipEnabled header
                                then boolRead (vp8ProbSkipFalse header) modeDec2
                                else (False, modeDec2)

                        if skip
                          then do
                            -- All coefficients are zero, just use prediction
                            let mbYBase = mbY * 16
                                mbXBase = mbX * 16
                            predict16x16 yMode yBuf (mbWidth * 16) mbXBase mbYBase
                            predict8x8 uvMode uBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                            predict8x8 uvMode vBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                            return (modeDec3, coeffDec)
                          else do
                            -- Decode Y2 block from DCT partition
                            (y2Coeffs, _, coeffDec1) <- decodeCoefficients coeffDec coeffProbs 1 0 0

                            -- Dequantize and apply WHT
                            let dequantFacts = computeDequantFactors (vp8QuantIndices header) (vp8Segments header)
                                dequantFact = dequantFacts V.! 0
                            dequantizeBlock dequantFact 1 y2Coeffs
                            iwht4x4 y2Coeffs

                            -- Decode and reconstruct 16 Y blocks from DCT partition
                            coeffDec2 <- reconstructMB16x16 yBuf mbY mbX mbWidth yMode y2Coeffs coeffDec1 coeffProbs dequantFact

                            -- Reconstruct U and V blocks from DCT partition
                            coeffDec3 <- reconstructChroma uBuf mbY mbX mbWidth uvMode coeffDec2 coeffProbs dequantFact 2
                            coeffDec4 <- reconstructChroma vBuf mbY mbX mbWidth uvMode coeffDec3 coeffProbs dequantFact 2
                            return (modeDec3, coeffDec4)

                  -- Continue to next macroblock with updated decoders
                  decodeMacroblocks mbY (mbX + 1) modeDecAfterMB coeffDecAfterMB

        (_finalModeDec, _finalCoeffDec) <- decodeMacroblocks 0 0 modeDecoder dctDecoder

        -- Apply loop filter to reconstructed frame
        when (vp8FilterLevel header > 0) $ do
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

-- | Reconstruct B_PRED macroblock (16 individual 4x4 blocks)
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
  ST s (BoolDecoder, BoolDecoder)
reconstructBPred yBuf mbY mbX mbStride modeDecoder coeffDecoder coeffProbs header = do
  let mbYBase = mbY * 16
      mbXBase = mbX * 16
      dequantFact = computeDequantFactors (vp8QuantIndices header) (vp8Segments header) V.! 0

  -- Decode each 4x4 block with its own mode
  let decodeBBlock blockIdx modeDec coeffDec = do
        let !by = blockIdx `shiftR` 2 -- div 4
            !bx = blockIdx .&. 3 -- mod 4
            blockY = mbYBase + by * 4
            blockX = mbXBase + bx * 4

        -- Read 4x4 intra mode from partition 0
        let probOffset = 0 * 10 * 9 + 0 * 9 -- above=0, left=0
            probs = V.convert $ VU.drop probOffset kfBmodeProbs
            (bMode, modeDec') = boolReadTree kfBmodeTree probs modeDec

        -- Apply 4x4 prediction
        predict4x4 bMode yBuf (mbStride * 16) blockX blockY

        -- Decode coefficients from DCT partition
        (coeffs, hasNonzero, coeffDec') <- decodeCoefficients coeffDec coeffProbs 0 0 0

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

  loopBBlocks 0 modeDecoder coeffDecoder

-- | Reconstruct 16x16 macroblock from coefficients (DCT partition)
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
  ST s BoolDecoder
reconstructMB16x16 yBuf mbY mbX mbStride yMode y2Coeffs decoder coeffProbs dequantFact = do
  let mbYBase = mbY * 16
      mbXBase = mbX * 16

  -- First apply prediction for the whole 16x16 block
  predict16x16 yMode yBuf (mbStride * 16) mbXBase mbYBase

  -- Decode and apply each 4x4 Y block
  let decodeYBlock blockIdx dec = do
        let !by = blockIdx `shiftR` 2 -- div 4
            !bx = blockIdx .&. 3 -- mod 4

        -- Decode coefficients for this 4x4 block from DCT partition
        (coeffs, hasNonzero, dec') <- decodeCoefficients dec coeffProbs 0 0 1 -- Block type 0 (Y after Y2), start at pos 1

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
            let yIdx = (mbYBase + by * 4 + dy) * mbStride * 16 + (mbXBase + bx * 4 + dx)
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

  loopYBlocks 0 decoder

-- | Reconstruct chroma blocks (U or V) from DCT partition
-- coeffBlockType should be 2 for U, 3 for V per RFC 6386 coefficient probability indexing
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
  ST s BoolDecoder
reconstructChroma uvBuf mbY mbX mbStride uvMode decoder coeffProbs dequantFact coeffBlockType = do
  let mbUVY = mbY * 8
      mbUVX = mbX * 8

  -- Apply prediction for 8x8 chroma block
  predict8x8 uvMode uvBuf (mbStride * 8) mbUVX mbUVY

  -- Decode and apply each 4x4 chroma block (4 blocks total for 8x8)
  let decodeUVBlock blockIdx dec = do
        let !by = blockIdx `shiftR` 1 -- div 2
            !bx = blockIdx .&. 1 -- mod 2

        -- Decode coefficients from DCT partition
        (coeffs, hasNonzero, dec') <- decodeCoefficients dec coeffProbs coeffBlockType 0 0

        -- Dequantize (always use type 2 = UV dequant for both U and V)
        dequantizeBlock dequantFact 2 coeffs

        -- Apply IDCT
        idct4x4 coeffs

        -- Add to prediction and clamp
        forM_ [0 :: Int .. 3] $ \dy ->
          forM_ [0 :: Int .. 3] $ \dx -> do
            let uvIdx = (mbUVY + by * 4 + dy) * mbStride * 8 + (mbUVX + bx * 4 + dx)
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

  loopUVBlocks 0 decoder

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
