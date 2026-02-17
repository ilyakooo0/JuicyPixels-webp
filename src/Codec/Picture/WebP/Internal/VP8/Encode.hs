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
      -- Filter level derived from quality: lower quality = more blocking = higher filter
      -- quality 100 → level 0, quality 50 → level 31, quality 0 → level 63
      encFilterLevel = min 63 $ max 0 $ (100 - quality) * 63 `div` 100,
      encFilterType = 1, -- Simple filter (faster, good enough for most cases)
      encUseSegmentation = False -- Disable segmentation
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
      dequantFactorsVec = computeDequantFactors quantIndices Nothing
      dequantFactors = dequantFactorsVec V.! 0 -- Single segment

  -- Step 3: Allocate reconstruction buffers (for prediction)
  yRecon <- VSM.replicate (paddedW * paddedH) 128
  uRecon <- VSM.replicate ((paddedW `div` 2) * (paddedH `div` 2)) 128
  vRecon <- VSM.replicate ((paddedW `div` 2) * (paddedH `div` 2)) 128

  -- Step 4: Generate compressed header (goes into partition 0)
  let compressedHeaderEnc = generateCompressedHeader quantIndices (encFilterLevel config) (encFilterType config)

  -- Step 5: Encode all macroblocks with SEPARATE streams for modes and coefficients
  -- Partition 0: compressed header + mode data
  -- DCT partition: coefficient data
  (finalModeEnc, finalCoeffEnc) <-
    encodeMacroblocks
      yBuf
      uBuf
      vBuf
      yRecon
      uRecon
      vRecon
      paddedW
      paddedH
      mbRows
      mbCols
      dequantFactors
      defaultCoeffProbs
      compressedHeaderEnc -- Mode encoder (continues from compressed header)
      initBoolEncoder -- Coefficient encoder (fresh)

  -- Step 6: Finalize both streams
  let partition0 = finalizeBoolEncoder finalModeEnc
      dctPartition = finalizeBoolEncoder finalCoeffEnc

  -- Step 7: Generate uncompressed header (firstPartSize = partition 0 only)
  let uncompHeader = generateUncompressedHeader width height (B.length partition0)

  -- With log2_nbr_of_dct_partitions=0: 1 DCT partition, 0 size entries
  -- Layout: [uncompressed header][partition 0][DCT partition]
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
  DequantFactors ->
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder -> -- Mode encoder (partition 0)
  BoolEncoder -> -- Coefficient encoder (DCT partition)
  ST s (BoolEncoder, BoolEncoder)
encodeMacroblocks yOrig uOrig vOrig yRecon uRecon vRecon paddedW paddedH mbRows mbCols dequantFactors coeffProbs modeEnc coeffEnc = do
  -- Allocate above NZ tracking arrays (persist across MB rows)
  aboveNzY <- VSM.replicate (mbCols * 4) (0 :: Word8) -- 4 Y columns per MB
  aboveNzU <- VSM.replicate (mbCols * 2) (0 :: Word8) -- 2 U columns per MB
  aboveNzV <- VSM.replicate (mbCols * 2) (0 :: Word8) -- 2 V columns per MB
  aboveNzDC <- VSM.replicate mbCols (0 :: Word8) -- 1 DC per MB
  let loop !mbY !mbX !mEnc !cEnc !leftNzY0 !leftNzY1 !leftNzY2 !leftNzY3 !leftNzU0 !leftNzU1 !leftNzV0 !leftNzV1 !leftNzDC
        | mbY >= mbRows = return (mEnc, cEnc)
        | mbX >= mbCols =
            -- New row: reset left NZ to 0
            loop (mbY + 1) 0 mEnc cEnc 0 0 0 0 0 0 0 0 0
        | otherwise = do
            (mEnc', cEnc', lY0, lY1, lY2, lY3, lU0, lU1, lV0, lV1, lDC) <-
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
                dequantFactors
                coeffProbs
                mEnc
                cEnc
                aboveNzY
                aboveNzU
                aboveNzV
                aboveNzDC
                leftNzY0
                leftNzY1
                leftNzY2
                leftNzY3
                leftNzU0
                leftNzU1
                leftNzV0
                leftNzV1
                leftNzDC
            loop mbY (mbX + 1) mEnc' cEnc' lY0 lY1 lY2 lY3 lU0 lU1 lV0 lV1 lDC

  loop 0 0 modeEnc coeffEnc 0 0 0 0 0 0 0 0 0

-- | Encode a single macroblock
-- Modes go to modeEnc (partition 0), coefficients go to coeffEnc (DCT partition)
-- Returns updated encoders and NZ state for left neighbor
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
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  BoolEncoder -> -- Mode and coefficient encoders
  VSM.MVector s Word8 -> -- aboveNzY (mbCols * 4)
  VSM.MVector s Word8 -> -- aboveNzU (mbCols * 2)
  VSM.MVector s Word8 -> -- aboveNzV (mbCols * 2)
  VSM.MVector s Word8 -> -- aboveNzDC (mbCols)
  Int ->
  Int ->
  Int ->
  Int -> -- leftNzY[0..3]
  Int ->
  Int -> -- leftNzU[0..1]
  Int ->
  Int -> -- leftNzV[0..1]
  Int -> -- leftNzDC
  ST s (BoolEncoder, BoolEncoder, Int, Int, Int, Int, Int, Int, Int, Int, Int)
encodeMacroblock yOrig uOrig vOrig yRecon uRecon vRecon paddedW _paddedH mbY mbX dequantFactors coeffProbs mEnc cEnc aboveNzY aboveNzU aboveNzV aboveNzDC leftNzY0 leftNzY1 leftNzY2 leftNzY3 leftNzU0 leftNzU1 leftNzV0 leftNzV1 leftNzDC = do
  let mbXpix = mbX * 16
      mbYpix = mbY * 16

  -- Step 1: Select best Y mode using SAD
  (predMode, _) <- selectIntra16x16Mode yOrig yRecon paddedW mbXpix mbYpix

  -- Step 2: Select best UV mode using SAD
  let chromaX = mbX * 8
      chromaY = mbY * 8
  (uvPredMode, _) <- selectChromaMode uOrig uRecon (paddedW `div` 2) chromaX chromaY

  -- Step 3: Write modes to partition 0
  let mEnc1 = encodeYMode predMode mEnc
      mEnc2 = encodeUVMode uvPredMode mEnc1

  -- Step 4: Encode Y blocks with NZ context tracking
  aNzDC <- VSM.read aboveNzDC mbX
  (cEnc1, y2nz, newLeftY0, newLeftY1, newLeftY2, newLeftY3) <-
    encodeYBlocks
      yOrig
      yRecon
      paddedW
      mbXpix
      mbYpix
      predMode
      dequantFactors
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

  -- Update above DC NZ
  VSM.write aboveNzDC mbX (if y2nz then 1 else 0)

  -- Step 5: Encode U blocks with NZ context
  (cEnc2, newLeftU0, newLeftU1) <-
    encodeChromaBlocks
      uOrig
      uRecon
      (paddedW `div` 2)
      chromaX
      chromaY
      uvPredMode
      dequantFactors
      coeffProbs
      cEnc1
      2
      aboveNzU
      mbX
      leftNzU0
      leftNzU1

  -- Step 6: Encode V blocks with NZ context
  (cEnc3, newLeftV0, newLeftV1) <-
    encodeChromaBlocks
      vOrig
      vRecon
      (paddedW `div` 2)
      chromaX
      chromaY
      uvPredMode
      dequantFactors
      coeffProbs
      cEnc2
      2
      aboveNzV
      mbX
      leftNzV0
      leftNzV1

  let newLeftDC = if y2nz then 1 else 0
  return (mEnc2, cEnc3, newLeftY0, newLeftY1, newLeftY2, newLeftY3, newLeftU0, newLeftU1, newLeftV0, newLeftV1, newLeftDC)

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
  ST s (BoolEncoder, Bool, Int, Int, Int, Int)
encodeYBlocks yOrig yRecon stride x y predMode dequantFactors coeffProbs enc aboveNzY mbX leftNzY0 leftNzY1 leftNzY2 leftNzY3 aboveDcNz leftDcNz = do
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

  -- Quantize Y2
  quantizeBlock dequantFactors 1 y2DCs

  -- ENCODE Y2 FIRST
  -- blockType=1 for Y2 (i16-DC per libwebp convention), ctx=0 (matches decoder)
  (enc1, y2nz) <- encodeCoefficients y2DCs coeffProbs 1 0 0 enc

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

            -- Quantize AC coefficients
            quantizeBlock dequantFactors 0 residuals

            -- Encode AC coefficients, ctx=0 (matches decoder)
            -- blockType=0 for Y AC (i16-AC per libwebp convention)
            (e', hasNz) <- encodeCoefficients residuals coeffProbs 0 0 1 e

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

    -- Get stored residuals (AC only, DC will come from Y2)
    residuals <- VSM.new 16
    forM_ [0 .. 15] $ \i -> do
      r <- VSM.read residualBlocks (blockIdx * 16 + i)
      VSM.write residuals i r

    -- Clear DC (it's handled by Y2)
    VSM.write residuals 0 0

    -- Quantize and dequantize AC coefficients
    quantizeBlock dequantFactors 0 residuals
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
  VU.Vector Word8 -> -- Coefficient probabilities
  BoolEncoder ->
  Int -> -- Coefficient block type (2 for both U and V)
  VSM.MVector s Word8 -> -- aboveNz (mbCols*2, read top row, write bottom row)
  Int -> -- mbX
  Int ->
  Int -> -- leftNz row 0, row 1
  ST s (BoolEncoder, Int, Int)
encodeChromaBlocks chromaOrig chromaRecon stride x y predMode dequantFactors coeffProbs enc coeffBlockType aboveNz mbX leftNz0 leftNz1 = do
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

            -- Quantize (always use type 2 = UV quant for both U and V)
            quantizeBlock dequantFactors 2 residuals

            -- Encode coefficients, ctx=0 (matches decoder)
            (e', hasNz) <- encodeCoefficients residuals coeffProbs coeffBlockType 0 0 e

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
