{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.Encode
  ( encodeVP8,
    EncodeConfig (..),
    defaultEncodeConfig,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.VP8.BoolEncoder
import Codec.Picture.WebP.Internal.VP8.ColorConvert
import Codec.Picture.WebP.Internal.VP8.DCT
import Codec.Picture.WebP.Internal.VP8.Dequant
import Codec.Picture.WebP.Internal.VP8.EncodeCoefficients
import Codec.Picture.WebP.Internal.VP8.EncodeHeader
import Codec.Picture.WebP.Internal.VP8.IDCT
import Codec.Picture.WebP.Internal.VP8.ModeSelection
import Codec.Picture.WebP.Internal.VP8.Predict
import Codec.Picture.WebP.Internal.VP8.Quantize
import Codec.Picture.WebP.Internal.VP8.Tables
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
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
  EncodeConfig
    { encQuality = quality,
      encFilterLevel = 0, -- No loop filter for simplicity
      encFilterType = 1, -- Simple (unused if level=0)
      encUseSegmentation = False -- Disable segmentation
    }

-- | Encode an RGB8 image to VP8 bitstream
-- Returns the raw VP8 data (without WebP container)
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
      dequantFactorsVec = computeDequantFactors quantIndices Nothing
      dequantFactors = dequantFactorsVec V.! 0 -- Single segment

  -- Step 3: Allocate reconstruction buffers (for prediction)
  yRecon <- VSM.replicate (paddedW * paddedH) 128
  uRecon <- VSM.replicate ((paddedW `div` 2) * (paddedH `div` 2)) 128
  vRecon <- VSM.replicate ((paddedW `div` 2) * (paddedH `div` 2)) 128

  -- Step 4: Generate compressed header
  let compressedHeaderEnc = generateCompressedHeader quantIndices (encFilterLevel config) (encFilterType config)
      compressedHeaderData = finalizeBoolEncoder compressedHeaderEnc

  -- Step 5: Initialize encoder for macroblock data
  let mbEncoder = initBoolEncoder

  -- Step 6: Encode all macroblocks
  finalEncoder <- encodeMacroblocks
    yBuf uBuf vBuf
    yRecon uRecon vRecon
    paddedW paddedH
    mbRows mbCols
    dequantFactors
    defaultCoeffProbs
    mbEncoder

  let mbData = finalizeBoolEncoder finalEncoder
      partition0 = compressedHeaderData <> mbData

  -- Step 7: Generate uncompressed header
  let uncompHeader = generateUncompressedHeader width height (B.length partition0)

  return $ uncompHeader <> partition0

-- | Encode all macroblocks
encodeMacroblocks ::
  VSM.MVector s Word8 -> -- Y original
  VSM.MVector s Word8 -> -- U original
  VSM.MVector s Word8 -> -- V original
  VSM.MVector s Word8 -> -- Y reconstruction
  VSM.MVector s Word8 -> -- U reconstruction
  VSM.MVector s Word8 -> -- V reconstruction
  Int -> Int -> -- Padded width, height
  Int -> Int -> -- MB rows, cols
  DequantFactors ->
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  ST s BoolEncoder
encodeMacroblocks yOrig uOrig vOrig yRecon uRecon vRecon paddedW paddedH mbRows mbCols dequantFactors coeffProbs encoder = do
  let loop !mbY !mbX !enc
        | mbY >= mbRows = return enc
        | mbX >= mbCols = loop (mbY + 1) 0 enc
        | otherwise = do
            enc' <- encodeMacroblock
              yOrig uOrig vOrig
              yRecon uRecon vRecon
              paddedW paddedH
              mbY mbX
              dequantFactors
              coeffProbs
              enc
            loop mbY (mbX + 1) enc'

  loop 0 0 encoder

-- | Encode a single macroblock
encodeMacroblock ::
  VSM.MVector s Word8 -> -- Y original
  VSM.MVector s Word8 -> -- U original
  VSM.MVector s Word8 -> -- V original
  VSM.MVector s Word8 -> -- Y reconstruction
  VSM.MVector s Word8 -> -- U reconstruction
  VSM.MVector s Word8 -> -- V reconstruction
  Int -> Int -> -- Padded width, height
  Int -> Int -> -- MB row, col
  DequantFactors ->
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  ST s BoolEncoder
encodeMacroblock yOrig uOrig vOrig yRecon uRecon vRecon paddedW paddedH mbY mbX dequantFactors coeffProbs enc = do
  let mbXpix = mbX * 16
      mbYpix = mbY * 16

  -- Step 1: Select Y mode (simplified: always use DC_PRED)
  -- Prediction uses modes 0-3 (DC/V/H/TM)
  -- Tree encoding uses values 1-4 (DC=1, V=2, H=3, TM missing, B_PRED=4)
  -- Map prediction mode to tree value: add 1
  let predMode = 0 -- DC_PRED for prediction
      yMode = predMode + 1 -- Tree value for encoding (DC_PRED = 1)

  -- Step 2: Select UV mode (simplified: always use DC_PRED)
  let uvPredMode = 0 -- DC_PRED for prediction
      uvMode = uvPredMode + 1 -- Tree value for encoding

  -- Step 3: Write modes to bitstream
  -- Hardcoded bit patterns for DC_PRED mode (temporary workaround)
  -- Y mode DC_PRED (value 1): bits "10" with probs [145, 156]
  -- First bit: 1 (right from root to skip B_PRED)
  -- Second bit: 0 (left to get DC_PRED)
  let enc1a = boolWrite 145 True enc  -- Y mode: First bit = 1
      enc1b = boolWrite 156 False enc1a -- Y mode: Second bit = 0

  -- UV mode DC_PRED (value 1): bits "10" with probs [142, 114]
      enc2a = boolWrite 142 True enc1b  -- UV mode: First bit = 1
      enc2 = boolWrite 114 False enc2a -- UV mode: Second bit = 0

  -- Step 4: Encode Y blocks (non-B_PRED mode: use Y2)
  -- Predict 16x16 block (use prediction mode, not tree value)
  predict16x16 predMode yRecon paddedW mbXpix mbYpix

  -- Compute residuals and encode
  enc3 <- encodeYBlocks yOrig yRecon paddedW mbXpix mbYpix dequantFactors coeffProbs enc2

  -- Step 5: Encode U blocks
  let chromaW = paddedW `div` 2
      chromaX = mbX * 8
      chromaY = mbY * 8

  predict8x8 uvPredMode uRecon chromaW chromaX chromaY
  enc4 <- encodeChromaBlocks uOrig uRecon chromaW chromaX chromaY dequantFactors coeffProbs enc3 2

  -- Step 6: Encode V blocks
  predict8x8 uvPredMode vRecon chromaW chromaX chromaY
  enc5 <- encodeChromaBlocks vOrig vRecon chromaW chromaX chromaY dequantFactors coeffProbs enc4 2

  return enc5

-- | Encode Y blocks for a macroblock (16x16)
encodeYBlocks ::
  VSM.MVector s Word8 -> -- Y original
  VSM.MVector s Word8 -> -- Y reconstruction (contains prediction)
  Int -> -- Stride
  Int -> Int -> -- X, Y position
  DequantFactors ->
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  ST s BoolEncoder
encodeYBlocks yOrig yRecon stride x y dequantFactors coeffProbs enc = do
  -- Collect 16 Y block DCs for Y2
  y2DCs <- VSM.new 16

  -- Process 16 Y blocks (4x4 each)
  let processBlock !blockIdx !e
        | blockIdx >= 16 = return e
        | otherwise = do
            let subX = (blockIdx `mod` 4) * 4
                subY = (blockIdx `div` 4) * 4

            -- Allocate residual block
            residuals <- VSM.new 16

            -- Compute residuals (original - prediction)
            forM_ [0 .. 3] $ \row ->
              forM_ [0 .. 3] $ \col -> do
                let px = x + subX + col
                    py = y + subY + row
                    idx = py * stride + px
                orig <- VSM.read yOrig idx
                pred <- VSM.read yRecon idx
                let residual = fromIntegral orig - fromIntegral pred :: Int16
                VSM.write residuals (row * 4 + col) residual

            -- Forward DCT
            fdct4x4 residuals

            -- Extract and store DC
            dc <- VSM.read residuals 0
            VSM.write y2DCs blockIdx dc
            VSM.write residuals 0 0 -- Clear DC (will be in Y2)

            -- Quantize AC coefficients
            quantizeBlock dequantFactors 0 residuals

            -- Encode AC coefficients (block type 0, start at position 1 since DC is in Y2)
            (e', _) <- encodeCoefficients residuals coeffProbs 0 0 1 e

            -- Reconstruct for future predictions
            dequantizeBlock dequantFactors 0 residuals
            idct4x4 residuals

            -- Add prediction back and clip
            forM_ [0 .. 3] $ \row ->
              forM_ [0 .. 3] $ \col -> do
                let px = x + subX + col
                    py = y + subY + row
                    idx = py * stride + px
                pred <- VSM.read yRecon idx
                res <- VSM.read residuals (row * 4 + col)
                let reconstructed = clip255 (fromIntegral pred + fromIntegral res)
                VSM.write yRecon idx reconstructed

            processBlock (blockIdx + 1) e'

  enc1 <- processBlock 0 enc

  -- Forward WHT on Y2 DCs
  fwht4x4 y2DCs

  -- Quantize Y2
  quantizeBlock dequantFactors 1 y2DCs

  -- Encode Y2 coefficients (block type 1)
  (enc2, _) <- encodeCoefficients y2DCs coeffProbs 1 0 0 enc1

  return enc2

-- | Encode chroma blocks (U or V)
encodeChromaBlocks ::
  VSM.MVector s Word8 -> -- Chroma original (U or V)
  VSM.MVector s Word8 -> -- Chroma reconstruction
  Int -> -- Stride
  Int -> Int -> -- X, Y position
  DequantFactors ->
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  Int -> -- Block type (2 for UV)
  ST s BoolEncoder
encodeChromaBlocks chromaOrig chromaRecon stride x y dequantFactors coeffProbs enc blockType = do
  -- Process 4 chroma blocks (4x4 each)
  let processBlock !blockIdx !e
        | blockIdx >= 4 = return e
        | otherwise = do
            let subX = (blockIdx `mod` 2) * 4
                subY = (blockIdx `div` 2) * 4

            -- Allocate residual block
            residuals <- VSM.new 16

            -- Compute residuals
            forM_ [0 .. 3] $ \row ->
              forM_ [0 .. 3] $ \col -> do
                let px = x + subX + col
                    py = y + subY + row
                    idx = py * stride + px
                orig <- VSM.read chromaOrig idx
                pred <- VSM.read chromaRecon idx
                let residual = fromIntegral orig - fromIntegral pred :: Int16
                VSM.write residuals (row * 4 + col) residual

            -- Forward DCT
            fdct4x4 residuals

            -- Quantize
            quantizeBlock dequantFactors blockType residuals

            -- Encode coefficients
            (e', _) <- encodeCoefficients residuals coeffProbs blockType 0 0 e

            -- Reconstruct
            dequantizeBlock dequantFactors blockType residuals
            idct4x4 residuals

            -- Add prediction back
            forM_ [0 .. 3] $ \row ->
              forM_ [0 .. 3] $ \col -> do
                let px = x + subX + col
                    py = y + subY + row
                    idx = py * stride + px
                pred <- VSM.read chromaRecon idx
                res <- VSM.read residuals (row * 4 + col)
                let reconstructed = clip255 (fromIntegral pred + fromIntegral res)
                VSM.write chromaRecon idx reconstructed

            processBlock (blockIdx + 1) e'

  processBlock 0 enc
