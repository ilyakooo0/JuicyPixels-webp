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
import Data.Int (Int8, Int16)
import qualified Data.ByteString as B
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

  -- Simple implementation: decode macroblocks and apply basic transforms
  let pixelData = runST $ do
        -- Allocate YUV buffers
        yBuf <- VSM.replicate (mbWidth * 16 * mbHeight * 16) (128 :: Word8)
        uBuf <- VSM.replicate (mbWidth * 8 * mbHeight * 8) (128 :: Word8)
        vBuf <- VSM.replicate (mbWidth * 8 * mbHeight * 8) (128 :: Word8)

        -- Initialize decoder
        let decoder = initBoolDecoder (vp8FirstPartition header)
            coeffProbs = vp8CoeffProbs header

        -- Decode all macroblocks with mode reading and coefficient decoding
        let decodeMacroblocks !mbY !mbX !decoder
              | mbY >= mbHeight = return decoder
              | mbX >= mbWidth = decodeMacroblocks (mbY + 1) 0 decoder
              | otherwise = do
                  -- Read Y mode (luma prediction mode)
                  let (yMode, decoder1) = boolReadTree kfYModeTree kfYModeProbs decoder

                  -- Read UV mode
                  let (uvMode, decoder2) = boolReadTree kfUVModeTree kfUVModeProbs decoder1

                  -- Full coefficient-based reconstruction
                  decoderAfterMB <- if yMode == 4
                    then do
                      -- B_PRED: 16 4x4 blocks with individual modes
                      decoderBPred <- reconstructBPred yBuf mbY mbX mbWidth decoder2 coeffProbs header

                      -- Reconstruct U and V with 8x8 prediction
                      predict8x8 uvMode uBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                      predict8x8 uvMode vBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                      decoderU <- reconstructChroma uBuf mbY mbX mbWidth uvMode decoderBPred coeffProbs (computeDequantFactors (vp8QuantIndices header) (vp8Segments header) V.! 0)
                      decoderV <- reconstructChroma vBuf mbY mbX mbWidth uvMode decoderU coeffProbs (computeDequantFactors (vp8QuantIndices header) (vp8Segments header) V.! 0)

                      return decoderV
                    else do
                      -- Decode Y2 block (contains DC values for 16 Y blocks)
                      let (skip, decoder3) = if vp8SkipEnabled header
                                               then boolRead (vp8ProbSkipFalse header) decoder2
                                               else (False, decoder2)

                      if skip
                        then do
                          -- All coefficients are zero, just use prediction
                          let mbYBase = mbY * 16
                              mbXBase = mbX * 16
                          predict16x16 yMode yBuf (mbWidth * 16) mbXBase mbYBase
                          predict8x8 uvMode uBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                          predict8x8 uvMode vBuf (mbWidth * 8) (mbX * 8) (mbY * 8)
                          return decoder3
                        else do
                          -- Decode Y2 block (DC coefficients)
                          (y2Coeffs, _, decoder4) <- decodeCoefficients decoder3 coeffProbs 1 0 0

                          -- Dequantize and apply WHT
                          let dequantFacts = computeDequantFactors (vp8QuantIndices header) (vp8Segments header)
                              dequantFact = dequantFacts V.! 0
                          dequantizeBlock dequantFact 1 y2Coeffs
                          iwht4x4 y2Coeffs

                          -- Decode and reconstruct 16 Y blocks
                          decoder5 <- reconstructMB16x16 yBuf mbY mbX mbWidth yMode y2Coeffs decoder4 coeffProbs dequantFact

                          -- Reconstruct U and V blocks (4 blocks each)
                          decoder6 <- reconstructChroma uBuf mbY mbX mbWidth uvMode decoder5 coeffProbs dequantFact
                          decoder7 <- reconstructChroma vBuf mbY mbX mbWidth uvMode decoder6 coeffProbs dequantFact

                          return decoder7

                  -- Continue to next macroblock - USE THE UPDATED DECODER!
                  decodeMacroblocks mbY (mbX + 1) decoderAfterMB

        _finalDecoder <- decodeMacroblocks 0 0 decoder

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
            let yIdx = y * mbWidth * 16 + x
                uIdx = (y `div` 2) * mbWidth * 8 + (x `div` 2)
                vIdx = (y `div` 2) * mbWidth * 8 + (x `div` 2)

                yVal = fromIntegral (yData VS.! yIdx) :: Int
                uVal = fromIntegral (uData VS.! uIdx) :: Int
                vVal = fromIntegral (vData VS.! vIdx) :: Int

                -- YUV to RGB conversion (BT.601)
                r = clamp (yVal + ((360 * (vVal - 128)) `div` 256))
                g = clamp (yVal - ((88 * (uVal - 128) + 184 * (vVal - 128)) `div` 256))
                b = clamp (yVal + ((455 * (uVal - 128)) `div` 256))

                rgbIdx = (y * width + x) * 3

            VSM.write rgbBuf rgbIdx (fromIntegral r)
            VSM.write rgbBuf (rgbIdx + 1) (fromIntegral g)
            VSM.write rgbBuf (rgbIdx + 2) (fromIntegral b)

        VS.freeze rgbBuf

  return $ Image width height pixelData

-- | Reconstruct B_PRED macroblock (16 individual 4x4 blocks)
reconstructBPred :: VSM.MVector s Word8 -> Int -> Int -> Int
                 -> BoolDecoder -> VU.Vector Word8 -> VP8FrameHeader
                 -> ST s BoolDecoder
reconstructBPred yBuf mbY mbX mbStride decoder coeffProbs header = do
  let mbYBase = mbY * 16
      mbXBase = mbX * 16
      dequantFact = computeDequantFactors (vp8QuantIndices header) (vp8Segments header) V.! 0

  -- For B_PRED, we need to track above and left modes for context
  -- Simplified: use default context (mode 0 = DC_PRED)
  let defaultProbs = kfBmodeProbs  -- Flat vector, use default probabilities

  -- Decode each 4x4 block with its own mode
  let decodeBBlock blockIdx dec = do
        let by = blockIdx `div` 4
            bx = blockIdx `mod` 4
            blockY = mbYBase + by * 4
            blockX = mbXBase + bx * 4

        -- Read 4x4 intra mode for this block (using simplified context)
        -- In full implementation, would use above/left modes for context
        -- For now, use mode 0,0 (DC prediction for both neighbors)
        let probOffset = 0 * 10 * 9 + 0 * 9  -- above=0, left=0
            probs = V.convert $ VU.drop probOffset kfBmodeProbs  -- Convert to boxed vector
            (bMode, dec1) = boolReadTree kfBmodeTree probs dec

        -- Apply 4x4 prediction
        predict4x4 bMode yBuf (mbStride * 16) blockX blockY

        -- Decode coefficients
        (coeffs, hasNonzero, dec2) <- decodeCoefficients dec1 coeffProbs 3 0 0  -- Block type 3 (Y)

        -- Dequantize
        dequantizeBlock dequantFact 3 coeffs  -- Type 3: Y block with DC

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

        return dec2

  -- Decode all 16 4x4 blocks
  let loopBBlocks blockIdx dec
        | blockIdx >= 16 = return dec
        | otherwise = do
            dec' <- decodeBBlock blockIdx dec
            loopBBlocks (blockIdx + 1) dec'

  loopBBlocks 0 decoder

-- | Reconstruct 16x16 macroblock from coefficients
reconstructMB16x16 :: VSM.MVector s Word8 -> Int -> Int -> Int -> Int
                   -> VSM.MVector s Int16 -> BoolDecoder -> VU.Vector Word8
                   -> DequantFactors -> ST s BoolDecoder
reconstructMB16x16 yBuf mbY mbX mbStride yMode y2Coeffs decoder coeffProbs dequantFact = do
  let mbYBase = mbY * 16
      mbXBase = mbX * 16

  -- First apply prediction for the whole 16x16 block
  predict16x16 yMode yBuf (mbStride * 16) mbXBase mbYBase

  -- Decode and apply each 4x4 Y block
  let decodeYBlock blockIdx dec = do
        let by = blockIdx `div` 4
            bx = blockIdx `mod` 4

        -- Decode coefficients for this 4x4 block
        (coeffs, hasNonzero, dec') <- decodeCoefficients dec coeffProbs 3 0 1  -- Block type 3 (Y), start at pos 1 (DC is from Y2)

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

-- | Reconstruct chroma blocks (U or V)
reconstructChroma :: VSM.MVector s Word8 -> Int -> Int -> Int -> Int
                  -> BoolDecoder -> VU.Vector Word8 -> DequantFactors
                  -> ST s BoolDecoder
reconstructChroma uvBuf mbY mbX mbStride uvMode decoder coeffProbs dequantFact = do
  let mbUVY = mbY * 8
      mbUVX = mbX * 8

  -- Apply prediction for 8x8 chroma block
  predict8x8 uvMode uvBuf (mbStride * 8) mbUVX mbUVY

  -- Decode and apply each 4x4 chroma block (4 blocks total for 8x8)
  let decodeUVBlock blockIdx dec = do
        let by = blockIdx `div` 2
            bx = blockIdx `mod` 2

        -- Decode coefficients
        (coeffs, hasNonzero, dec') <- decodeCoefficients dec coeffProbs 2 0 0  -- Block type 2 (UV)

        -- DEBUG: Check if we got nonzero for our encoded file
        when (mbY == 0 && mbX == 0 && blockIdx == 0 && hasNonzero) $ do
          c0 <- VSM.read coeffs 0
          error $ "DEBUG DECODER: UV has nonzero! hasNonzero=True, coeffs[0]=" ++ show c0

        -- Dequantize
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

-- | Fill macroblock with simple values (helper function)
fillMBSimple :: VSM.MVector s Word8 -> VSM.MVector s Word8 -> VSM.MVector s Word8
             -> Int -> Int -> Int -> Word8 -> Word8 -> Word8 -> ST s ()
fillMBSimple yBuf uBuf vBuf mbY mbX mbWidth yVal uVal vVal = do
  let yBase = mbY * 16 * mbWidth * 16 + mbX * 16
      uBase = mbY * 8 * mbWidth * 8 + mbX * 8
      vBase = mbY * 8 * mbWidth * 8 + mbX * 8

  -- Fill Y plane
  forM_ [0 :: Int .. 15] $ \dy ->
    forM_ [0 :: Int .. 15] $ \dx -> do
      let yIdx = yBase + dy * mbWidth * 16 + dx
      VSM.write yBuf yIdx yVal

  -- Fill U,V planes
  forM_ [0 :: Int .. 7] $ \dy ->
    forM_ [0 :: Int .. 7] $ \dx -> do
      let uIdx = uBase + dy * mbWidth * 8 + dx
          vIdx = vBase + dy * mbWidth * 8 + dx
      VSM.write uBuf uIdx uVal
      VSM.write vBuf vIdx vVal

-- | Clamp value to 0-255 range
clamp :: Int -> Int
clamp x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = x

-- Keyframe Y mode tree (from RFC 6386)
kfYModeTree :: V.Vector Int8
kfYModeTree = V.fromList
  [ -4,  2,   -- B_PRED at index 0 (code "0")
     -1,  4,   -- DC_PRED at index 2
     -2,  6,   -- V_PRED at index 4
     -3,  8    -- H_PRED at index 6, TM_PRED at index 8
  ]

-- Keyframe Y mode probabilities
kfYModeProbs :: V.Vector Word8
kfYModeProbs = V.fromList [145, 156, 163, 128]

-- Keyframe UV mode tree
kfUVModeTree :: V.Vector Int8
kfUVModeTree = V.fromList
  [ -1,  2,   -- DC_PRED
     -2,  4,   -- V_PRED
     -3, -4    -- H_PRED, TM_PRED
  ]

-- Keyframe UV mode probabilities
kfUVModeProbs :: V.Vector Word8
kfUVModeProbs = V.fromList [142, 114, 183]
