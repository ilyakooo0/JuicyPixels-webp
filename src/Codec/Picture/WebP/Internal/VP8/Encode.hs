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
import Codec.Picture.WebP.Internal.VP8.EncodeMode
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

  -- Step 4: Generate compressed header and continue encoding with same BoolEncoder
  let compressedHeaderEnc = generateCompressedHeader quantIndices (encFilterLevel config) (encFilterType config)
      -- DO NOT finalize yet! Continue with same encoder for MB data

  -- Step 5: Encode all macroblocks using the SAME encoder (continues arithmetic coding stream)
  finalEncoder <- encodeMacroblocks
    yBuf uBuf vBuf
    yRecon uRecon vRecon
    paddedW paddedH
    mbRows mbCols
    dequantFactors
    defaultCoeffProbs
    compressedHeaderEnc  -- Continue from compressed header encoder!

  -- Step 6: Finalize the combined stream
  let partition0 = finalizeBoolEncoder finalEncoder

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

  -- Step 1: Select best Y mode using SAD
  (predMode, _) <- selectIntra16x16Mode yOrig yRecon paddedW mbXpix mbYpix
  -- Map prediction mode (0-3) to tree value (1-4): DC=1, V=2, H=3, TM=4
  let yMode = predMode + 1

  -- Step 2: Select best UV mode using SAD
  let chromaX = mbX * 8
      chromaY = mbY * 8
  (uvPredMode, _) <- selectChromaMode uOrig uRecon (paddedW `div` 2) chromaX chromaY
  let uvMode = uvPredMode + 1

  -- Step 3: Write modes to bitstream using selected modes
  let enc1 = encodeYMode predMode enc
      enc2 = encodeUVMode uvPredMode enc1

  -- Step 4: Encode Y blocks (non-B_PRED mode: use Y2)
  -- Prediction was already done during mode selection, now encode
  enc3 <- encodeYBlocks yOrig yRecon paddedW mbXpix mbYpix predMode dequantFactors coeffProbs enc2

  -- Step 5: Encode U blocks
  enc4 <- encodeChromaBlocks uOrig uRecon (paddedW `div` 2) chromaX chromaY uvPredMode dequantFactors coeffProbs enc3 2

  -- Step 6: Encode V blocks
  enc5 <- encodeChromaBlocks vOrig vRecon (paddedW `div` 2) chromaX chromaY uvPredMode dequantFactors coeffProbs enc4 2

  return enc5

-- | Encode Y blocks for a macroblock (16x16)
encodeYBlocks ::
  VSM.MVector s Word8 -> -- Y original
  VSM.MVector s Word8 -> -- Y reconstruction (will contain prediction)
  Int -> -- Stride
  Int -> Int -> -- X, Y position
  Int -> -- Prediction mode (0-3)
  DequantFactors ->
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  ST s BoolEncoder
encodeYBlocks yOrig yRecon stride x y predMode dequantFactors coeffProbs enc = do
  -- Create temporary buffer for prediction (don't overwrite reconstruction yet)
  predBuf <- VSM.clone yRecon

  -- Apply prediction to temporary buffer
  predict16x16 predMode predBuf stride x y

  -- Collect 16 Y block DCs for Y2 by doing forward DCT on all blocks
  y2DCs <- VSM.new 16
  residualBlocks <- VSM.new (16 * 16)  -- Store all 16 blocks for later encoding

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

  -- Quantize Y2
  quantizeBlock dequantFactors 1 y2DCs

  -- ENCODE Y2 FIRST (this is what decoder expects!)
  (enc1, _) <- encodeCoefficients y2DCs coeffProbs 1 0 0 enc

  -- Now encode 16 Y blocks (AC only, DC is in Y2)
  let encodeYBlock !blockIdx !e
        | blockIdx >= 16 = return e
        | otherwise = do
            -- Get stored residuals for this block
            residuals <- VSM.new 16
            forM_ [0 .. 15] $ \i -> do
              r <- VSM.read residualBlocks (blockIdx * 16 + i)
              VSM.write residuals i r

            -- DC was already extracted for Y2, clear it
            VSM.write residuals 0 0

            -- Quantize AC coefficients
            quantizeBlock dequantFactors 0 residuals

            -- Encode AC coefficients (start at position 1, DC is in Y2)
            (e', _) <- encodeCoefficients residuals coeffProbs 0 0 1 e

            encodeYBlock (blockIdx + 1) e'

  enc2 <- encodeYBlock 0 enc1

  -- Now reconstruct all Y blocks for future predictions
  forM_ [0 .. 15] $ \blockIdx -> do
    let subX = (blockIdx `mod` 4) * 4
        subY = (blockIdx `div` 4) * 4

    -- Get stored residuals
    residuals <- VSM.new 16
    forM_ [0 .. 15] $ \i -> do
      r <- VSM.read residualBlocks (blockIdx * 16 + i)
      VSM.write residuals i r

    -- Set DC from Y2 (need to get it from dequantized Y2)
    -- For reconstruction, use quantized then dequantized values
    -- This is complex - for now, reconstruct from original residuals

    -- Quantize, dequantize, IDCT
    VSM.write residuals 0 0  -- DC handled by Y2
    quantizeBlock dequantFactors 0 residuals
    dequantizeBlock dequantFactors 0 residuals
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

  return enc2

-- | Encode chroma blocks (U or V)
encodeChromaBlocks ::
  VSM.MVector s Word8 -> -- Chroma original (U or V)
  VSM.MVector s Word8 -> -- Chroma reconstruction
  Int -> -- Stride
  Int -> Int -> -- X, Y position
  Int -> -- Prediction mode (0-3)
  DequantFactors ->
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  Int -> -- Block type (2 for UV)
  ST s BoolEncoder
encodeChromaBlocks chromaOrig chromaRecon stride x y predMode dequantFactors coeffProbs enc blockType = do
  -- Create temporary buffer for prediction
  predBuf <- VSM.clone chromaRecon

  -- Apply prediction to temporary buffer
  predict8x8 predMode predBuf stride x y

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
                pred <- VSM.read predBuf idx  -- Use prediction buffer
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

            -- Add prediction back and write to reconstruction buffer
            forM_ [0 .. 3] $ \row ->
              forM_ [0 .. 3] $ \col -> do
                let px = x + subX + col
                    py = y + subY + row
                    idx = py * stride + px
                pred <- VSM.read predBuf idx  -- Get prediction value
                res <- VSM.read residuals (row * 4 + col)
                let reconstructed = clip255 (fromIntegral pred + fromIntegral res)
                VSM.write chromaRecon idx reconstructed  -- Write to reconstruction for future MBs

            processBlock (blockIdx + 1) e'

  processBlock 0 enc
