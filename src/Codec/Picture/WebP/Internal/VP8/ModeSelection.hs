{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.ModeSelection
  ( selectIntra16x16Mode,
    selectIntra4x4Mode,
    selectChromaMode,
    selectIntra16x16ModeRDO,
    selectBPredModeRDO,
    selectChromaModeRDO,
  )
where

import Codec.Picture.WebP.Internal.VP8.ColorConvert (clip255)
import Codec.Picture.WebP.Internal.VP8.DCT (fdct4x4, fwht4x4)
import Codec.Picture.WebP.Internal.VP8.Dequant (DequantFactors, dequantizeBlock)
import Codec.Picture.WebP.Internal.VP8.IDCT (idct4x4, iwht4x4)
import Codec.Picture.WebP.Internal.VP8.Predict
import Codec.Picture.WebP.Internal.VP8.Quantize (applySharpen, trellisQuantizeBlock)
import Codec.Picture.WebP.Internal.VP8.RateCost
  ( bPredYModeCost,
    bSubModeCost,
    coeffBlockCost,
    i16ModeCost,
    uvModeCost,
  )
import Control.Monad.ST
import Data.Bits (shiftR, (.&.))
import Data.Int (Int16)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- Prediction modes (from Tables.hs and Predict.hs)
-- 16x16 luma modes: DC_PRED=0, V_PRED=1, H_PRED=2, TM_PRED=3
-- 4x4 luma modes: B_DC_PRED=0, B_TM_PRED=1, B_VE_PRED=2, B_HE_PRED=3, ...
-- 8x8 chroma modes: DC_PRED=0, V_PRED=1, H_PRED=2, TM_PRED=3

-- | Early exit threshold - if SAD is below this, stop searching
-- This represents a "good enough" match that's unlikely to be beaten
earlyExitThreshold16x16 :: Int
earlyExitThreshold16x16 = 128 -- ~0.5 per pixel on average
{-# INLINE earlyExitThreshold16x16 #-}

earlyExitThreshold8x8 :: Int
earlyExitThreshold8x8 = 32 -- ~0.5 per pixel on average
{-# INLINE earlyExitThreshold8x8 #-}

earlyExitThreshold4x4 :: Int
earlyExitThreshold4x4 = 8 -- ~0.5 per pixel on average
{-# INLINE earlyExitThreshold4x4 #-}

-- | Select best 16x16 intra prediction mode using SAD
-- Returns (mode, sad) where mode is 0-3 (DC/V/H/TM)
-- Optimized: single prediction buffer, early exit, incremental SAD
{-# INLINE selectIntra16x16Mode #-}
selectIntra16x16Mode ::
  VSM.MVector s Word8 -> -- Y plane original buffer
  VSM.MVector s Word8 -> -- Y plane reconstruction buffer (for prediction)
  Int -> -- Stride (width of Y plane)
  Int -> -- Macroblock X position (in pixels)
  Int -> -- Macroblock Y position (in pixels)
  ST s (Int, Int) -- (mode, sad)
selectIntra16x16Mode yOrig yRecon stride mbX mbY = do
  -- Allocate single temporary prediction buffer (reused for all modes)
  predBuf <- VSM.clone yRecon

  -- Try modes in order of typical efficiency: DC, V, H, TM
  -- Start with DC (mode 0) as baseline
  predict16x16 0 predBuf stride mbX mbY
  !sad0 <- computeSAD16x16Fast yOrig predBuf stride mbX mbY

  -- Early exit if DC mode is excellent
  if sad0 < earlyExitThreshold16x16
    then return (0, sad0)
    else do
      -- Try V_PRED (mode 1)
      predict16x16 1 predBuf stride mbX mbY
      !sad1 <- computeSAD16x16Fast yOrig predBuf stride mbX mbY

      let (!best1, !bestSad1) = if sad1 < sad0 then (1, sad1) else (0, sad0)

      if bestSad1 < earlyExitThreshold16x16
        then return (best1, bestSad1)
        else do
          -- Try H_PRED (mode 2)
          predict16x16 2 predBuf stride mbX mbY
          !sad2 <- computeSAD16x16Fast yOrig predBuf stride mbX mbY

          let (!best2, !bestSad2) = if sad2 < bestSad1 then (2, sad2) else (best1, bestSad1)

          if bestSad2 < earlyExitThreshold16x16
            then return (best2, bestSad2)
            else do
              -- Try TM_PRED (mode 3)
              predict16x16 3 predBuf stride mbX mbY
              !sad3 <- computeSAD16x16Fast yOrig predBuf stride mbX mbY

              let (!best3, !bestSad3) = if sad3 < bestSad2 then (3, sad3) else (best2, bestSad2)
              return (best3, bestSad3)

-- | Select best 4x4 intra prediction mode for a sub-block
-- Returns (mode, sad) where mode is 0-9 (B_DC_PRED through B_HU_PRED)
-- Optimized: single buffer, early exit, strict loop
{-# INLINE selectIntra4x4Mode #-}
selectIntra4x4Mode ::
  VSM.MVector s Word8 -> -- Y plane original buffer
  VSM.MVector s Word8 -> -- Y plane reconstruction buffer
  Int -> -- Stride
  Int -> -- Macroblock X (in pixels)
  Int -> -- Macroblock Y (in pixels)
  Int -> -- Sub-block index (0-15)
  ST s (Int, Int)
selectIntra4x4Mode yOrig yRecon stride mbX mbY subBlock = do
  let !subX = mbX + (subBlock .&. 3) * 4 -- subBlock `mod` 4 as bit op
      !subY = mbY + (subBlock `shiftR` 2) * 4 -- subBlock `div` 4 as shift

  -- Allocate single temporary prediction buffer
  predBuf <- VSM.clone yRecon

  -- Try all 10 modes with early exit
  let go !mode !bestMode !bestSAD
        | mode > 9 = return (bestMode, bestSAD)
        | bestSAD == 0 = return (bestMode, bestSAD) -- Can't do better than 0
        | otherwise = do
            predict4x4 mode predBuf stride subX subY
            !sad <- computeSAD4x4Fast yOrig predBuf stride subX subY
            if sad < bestSAD
              then
                if sad < earlyExitThreshold4x4
                  then return (mode, sad) -- Early exit
                  else go (mode + 1) mode sad
              else go (mode + 1) bestMode bestSAD

  -- Start with mode 0
  predict4x4 0 predBuf stride subX subY
  !sad0 <- computeSAD4x4Fast yOrig predBuf stride subX subY
  if sad0 < earlyExitThreshold4x4
    then return (0, sad0)
    else go 1 0 sad0

-- | Select best chroma (U/V) 8x8 intra prediction mode
-- Returns (mode, sad) where mode is 0-3 (DC/V/H/TM)
-- Optimized: single buffer, early exit
{-# INLINE selectChromaMode #-}
selectChromaMode ::
  VSM.MVector s Word8 -> -- U or V plane original buffer
  VSM.MVector s Word8 -> -- U or V plane reconstruction buffer
  Int -> -- Stride (width of chroma plane)
  Int -> -- Macroblock X (in pixels, chroma coordinates)
  Int -> -- Macroblock Y (in pixels, chroma coordinates)
  ST s (Int, Int)
selectChromaMode chromaOrig chromaRecon stride mbX mbY = do
  predBuf <- VSM.clone chromaRecon

  -- Try DC (mode 0) first
  predict8x8 0 predBuf stride mbX mbY
  !sad0 <- computeSAD8x8Fast chromaOrig predBuf stride mbX mbY

  if sad0 < earlyExitThreshold8x8
    then return (0, sad0)
    else do
      -- Try V_PRED (mode 1)
      predict8x8 1 predBuf stride mbX mbY
      !sad1 <- computeSAD8x8Fast chromaOrig predBuf stride mbX mbY

      let (!best1, !bestSad1) = if sad1 < sad0 then (1, sad1) else (0, sad0)

      if bestSad1 < earlyExitThreshold8x8
        then return (best1, bestSad1)
        else do
          -- Try H_PRED (mode 2)
          predict8x8 2 predBuf stride mbX mbY
          !sad2 <- computeSAD8x8Fast chromaOrig predBuf stride mbX mbY

          let (!best2, !bestSad2) = if sad2 < bestSad1 then (2, sad2) else (best1, bestSad1)

          if bestSad2 < earlyExitThreshold8x8
            then return (best2, bestSad2)
            else do
              -- Try TM_PRED (mode 3)
              predict8x8 3 predBuf stride mbX mbY
              !sad3 <- computeSAD8x8Fast chromaOrig predBuf stride mbX mbY

              let (!best3, !bestSad3) = if sad3 < bestSad2 then (3, sad3) else (best2, bestSad2)
              return (best3, bestSad3)

-- | Compute Sum of Absolute Differences for 16x16 block
-- Optimized with pre-computed row bases, unsafeRead, and strict accumulation
{-# INLINE computeSAD16x16Fast #-}
computeSAD16x16Fast ::
  VSM.MVector s Word8 -> -- Original
  VSM.MVector s Word8 -> -- Prediction
  Int -> -- Stride
  Int ->
  Int -> -- X, Y position
  ST s Int
computeSAD16x16Fast orig pred_ stride x y = do
  let !baseY = y * stride + x

  -- Unroll outer loop for better performance
  let goRow !row !acc
        | row >= 16 = return acc
        | otherwise = do
            let !rowBase = baseY + row * stride
            -- Process 16 columns with manual unrolling (4 at a time)
            !acc0 <- goCol4 rowBase 0 acc
            !acc1 <- goCol4 rowBase 4 acc0
            !acc2 <- goCol4 rowBase 8 acc1
            !acc3 <- goCol4 rowBase 12 acc2
            goRow (row + 1) acc3

      goCol4 !rowBase !col !acc = do
        let !idx0 = rowBase + col
            !idx1 = idx0 + 1
            !idx2 = idx0 + 2
            !idx3 = idx0 + 3
        !o0 <- VSM.unsafeRead orig idx0
        !p0 <- VSM.unsafeRead pred_ idx0
        !o1 <- VSM.unsafeRead orig idx1
        !p1 <- VSM.unsafeRead pred_ idx1
        !o2 <- VSM.unsafeRead orig idx2
        !p2 <- VSM.unsafeRead pred_ idx2
        !o3 <- VSM.unsafeRead orig idx3
        !p3 <- VSM.unsafeRead pred_ idx3
        let !d0 = abs (fromIntegral o0 - fromIntegral p0 :: Int)
            !d1 = abs (fromIntegral o1 - fromIntegral p1 :: Int)
            !d2 = abs (fromIntegral o2 - fromIntegral p2 :: Int)
            !d3 = abs (fromIntegral o3 - fromIntegral p3 :: Int)
        return $! acc + d0 + d1 + d2 + d3

  goRow 0 0

-- | Compute SAD for 8x8 block - optimized version
{-# INLINE computeSAD8x8Fast #-}
computeSAD8x8Fast ::
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  ST s Int
computeSAD8x8Fast orig pred_ stride x y = do
  let !baseY = y * stride + x

  let goRow !row !acc
        | row >= 8 = return acc
        | otherwise = do
            let !rowBase = baseY + row * stride
            -- Process 8 columns with manual unrolling (4 at a time)
            !acc0 <- goCol4 rowBase 0 acc
            !acc1 <- goCol4 rowBase 4 acc0
            goRow (row + 1) acc1

      goCol4 !rowBase !col !acc = do
        let !idx0 = rowBase + col
            !idx1 = idx0 + 1
            !idx2 = idx0 + 2
            !idx3 = idx0 + 3
        !o0 <- VSM.unsafeRead orig idx0
        !p0 <- VSM.unsafeRead pred_ idx0
        !o1 <- VSM.unsafeRead orig idx1
        !p1 <- VSM.unsafeRead pred_ idx1
        !o2 <- VSM.unsafeRead orig idx2
        !p2 <- VSM.unsafeRead pred_ idx2
        !o3 <- VSM.unsafeRead orig idx3
        !p3 <- VSM.unsafeRead pred_ idx3
        let !d0 = abs (fromIntegral o0 - fromIntegral p0 :: Int)
            !d1 = abs (fromIntegral o1 - fromIntegral p1 :: Int)
            !d2 = abs (fromIntegral o2 - fromIntegral p2 :: Int)
            !d3 = abs (fromIntegral o3 - fromIntegral p3 :: Int)
        return $! acc + d0 + d1 + d2 + d3

  goRow 0 0

-- | Compute SAD for 4x4 block - optimized version
{-# INLINE computeSAD4x4Fast #-}
computeSAD4x4Fast ::
  VSM.MVector s Word8 ->
  VSM.MVector s Word8 ->
  Int ->
  Int ->
  Int ->
  ST s Int
computeSAD4x4Fast orig pred_ stride x y = do
  let !baseY = y * stride + x

  let goRow !row !acc
        | row >= 4 = return acc
        | otherwise = do
            let !rowBase = baseY + row * stride
            -- Process all 4 columns at once
            let !idx0 = rowBase
                !idx1 = rowBase + 1
                !idx2 = rowBase + 2
                !idx3 = rowBase + 3
            !o0 <- VSM.unsafeRead orig idx0
            !p0 <- VSM.unsafeRead pred_ idx0
            !o1 <- VSM.unsafeRead orig idx1
            !p1 <- VSM.unsafeRead pred_ idx1
            !o2 <- VSM.unsafeRead orig idx2
            !p2 <- VSM.unsafeRead pred_ idx2
            !o3 <- VSM.unsafeRead orig idx3
            !p3 <- VSM.unsafeRead pred_ idx3
            let !d0 = abs (fromIntegral o0 - fromIntegral p0 :: Int)
                !d1 = abs (fromIntegral o1 - fromIntegral p1 :: Int)
                !d2 = abs (fromIntegral o2 - fromIntegral p2 :: Int)
                !d3 = abs (fromIntegral o3 - fromIntegral p3 :: Int)
            goRow (row + 1) (acc + d0 + d1 + d2 + d3)

  goRow 0 0

-- ---------------------------------------------------------------------------
-- Rate-Distortion Optimized mode selection
-- ---------------------------------------------------------------------------

-- | Select best 16x16 mode using Rate-Distortion Optimization.
-- For each candidate mode: predict → DCT → quant → estimate bits → dequant → IDCT → SSE.
-- Score = SSE + (lambda * totalBitCost) / 256, where totalBitCost includes
-- coefficient encoding cost (from coeffBlockCost) and mode signaling cost.
{-# INLINE selectIntra16x16ModeRDO #-}
selectIntra16x16ModeRDO ::
  VSM.MVector s Word8 -> -- Y plane original buffer
  VSM.MVector s Word8 -> -- Y plane reconstruction buffer (for prediction context)
  Int -> -- Stride (padded width)
  Int -> -- Macroblock X position (pixels)
  Int -> -- Macroblock Y position (pixels)
  DequantFactors -> -- Quantization parameters
  Int -> -- Lambda (rate-distortion tradeoff)
  VU.Vector Word8 -> -- Coefficient probabilities (1056 flat entries)
  Int -> Int -> Int -> Int -> -- aboveNzY[0..3] (from MB above)
  Int -> Int -> Int -> Int -> -- leftNzY[0..3] (from MB to the left)
  Int -> Int -> -- aboveDcNz, leftDcNz (Y2 DC NZ context)
  ST s (Int, Int) -- (mode, rdCost)
selectIntra16x16ModeRDO yOrig yRecon stride mbX mbY dqFactors lambda coeffProbs aNzY0 aNzY1 aNzY2 aNzY3 lNzY0 lNzY1 lNzY2 lNzY3 aDcNz lDcNz = do
  predBuf <- VSM.clone yRecon
  y2DCs <- VSM.new 16
  residuals <- VSM.new 16
  dctStore <- VSM.new (16 * 16)
  nzGrid <- VSM.new 16 :: ST s (VSM.MVector s Word8)

  let tryMode !mode !bestMode !bestCost
        | mode > 3 = return (bestMode, bestCost)
        | otherwise = do
            predict16x16 mode predBuf stride mbX mbY

            -- First pass: compute residuals, DCT, collect DCs
            let collectDC !bi
                  | bi >= 16 = return ()
                  | otherwise = do
                      let !subX = (bi .&. 3) * 4
                          !subY = (bi `shiftR` 2) * 4
                      let fillRes !r
                            | r >= 4 = return ()
                            | otherwise = do
                                let fillCol !c
                                      | c >= 4 = fillRes (r + 1)
                                      | otherwise = do
                                          let !idx = (mbY + subY + r) * stride + (mbX + subX + c)
                                          !o <- VSM.unsafeRead yOrig idx
                                          !p <- VSM.unsafeRead predBuf idx
                                          VSM.unsafeWrite residuals (r * 4 + c) (fromIntegral o - fromIntegral p :: Int16)
                                          fillCol (c + 1)
                                fillCol 0
                      fillRes 0
                      fdct4x4 residuals
                      !dc <- VSM.unsafeRead residuals 0
                      VSM.unsafeWrite y2DCs bi dc
                      let storeCoeffs !i
                            | i >= 16 = collectDC (bi + 1)
                            | otherwise = do
                                !r <- VSM.unsafeRead residuals i
                                VSM.unsafeWrite dctStore (bi * 16 + i) r
                                storeCoeffs (i + 1)
                      storeCoeffs 0
            collectDC 0

            -- Y2: WHT → trellis quantize → estimate bit cost → dequant → inverse WHT
            fwht4x4 y2DCs
            let !dcCtx = min 2 (aDcNz + lDcNz)
            _ <- trellisQuantizeBlock dqFactors 1 y2DCs coeffProbs dcCtx 0 lambda
            !y2BitCost <- coeffBlockCost y2DCs coeffProbs 1 dcCtx 0
            dequantizeBlock dqFactors 1 y2DCs
            reconDCsV <- iwht4x4 y2DCs

            -- Clear NZ grid for this mode trial
            let clearNz !i
                  | i >= 16 = return ()
                  | otherwise = VSM.unsafeWrite nzGrid i 0 >> clearNz (i + 1)
            clearNz 0

            -- Second pass: quantize AC, estimate bits, reconstruct, compute SSE
            let processBlock !bi !sse !rateCost
                  | bi >= 16 = do
                      let !modeBitCost = i16ModeCost mode
                          !rdCost = sse + (lambda * (rateCost + modeBitCost)) `div` 256
                      if rdCost < bestCost
                        then tryMode (mode + 1) mode rdCost
                        else tryMode (mode + 1) bestMode bestCost
                  | otherwise = do
                      let !row = bi `shiftR` 2
                          !col = bi .&. 3
                          !subX = col * 4
                          !subY = row * 4
                      -- Compute NZ context from above/left neighbors
                      !aboveNz <- if row == 0
                        then return $ case col of
                          0 -> aNzY0; 1 -> aNzY1; 2 -> aNzY2; _ -> aNzY3
                        else fromIntegral <$> VSM.unsafeRead nzGrid (bi - 4)
                      !leftNz <- if col == 0
                        then return $ case row of
                          0 -> lNzY0; 1 -> lNzY1; 2 -> lNzY2; _ -> lNzY3
                        else fromIntegral <$> VSM.unsafeRead nzGrid (bi - 1)
                      let !ctx = min 2 (aboveNz + leftNz)
                      let loadCoeffs !i
                            | i >= 16 = return ()
                            | otherwise = do
                                !r <- VSM.unsafeRead dctStore (bi * 16 + i)
                                VSM.unsafeWrite residuals i r
                                loadCoeffs (i + 1)
                      loadCoeffs 0
                      VSM.unsafeWrite residuals 0 0
                      applySharpen dqFactors 0 residuals
                      !hasNz <- trellisQuantizeBlock dqFactors 0 residuals coeffProbs ctx 1 lambda
                      !blockBitCost <- coeffBlockCost residuals coeffProbs 0 ctx 1
                      VSM.unsafeWrite nzGrid bi (if hasNz then 1 else 0)
                      dequantizeBlock dqFactors 0 residuals
                      VSM.unsafeWrite residuals 0 (reconDCsV VS.! bi)
                      idct4x4 residuals
                      !blockSSE <- computeBlockSSEM yOrig predBuf residuals stride (mbX + subX) (mbY + subY)
                      processBlock (bi + 1) (sse + blockSSE) (rateCost + blockBitCost)

            processBlock 0 0 y2BitCost

  tryMode 0 0 maxBound

-- | Select best 4x4 modes for all 16 sub-blocks of a B_PRED macroblock.
-- For each sub-block (in raster order): tries all 10 modes with true RDO
-- (coefficient bit cost + mode signaling cost), picks the best, then
-- reconstructs into yRecon so subsequent blocks can predict from it.
-- Returns (16 modes as Word8, total RD cost including B_PRED Y mode signal).
-- WARNING: Modifies yRecon in place!
selectBPredModeRDO ::
  VSM.MVector s Word8 -> -- Y plane original
  VSM.MVector s Word8 -> -- Y plane reconstruction (MODIFIED!)
  Int -> -- Stride (padded width)
  Int -> -- MB X position (pixels)
  Int -> -- MB Y position (pixels)
  DequantFactors ->
  Int -> -- Lambda
  VU.Vector Word8 -> -- Coefficient probabilities (1056 flat entries)
  Int ->
  Int ->
  Int ->
  Int -> -- Above B-modes from MB above (cols 0-3)
  Int ->
  Int ->
  Int ->
  Int -> -- Left B-modes from MB to the left (rows 0-3)
  Int -> Int -> Int -> Int -> -- aboveNzY[0..3] (from MB above)
  Int -> Int -> Int -> Int -> -- leftNzY[0..3] (from MB to the left)
  ST s (VS.Vector Word8, Int) -- (16 modes, total RD cost)
selectBPredModeRDO yOrig yRecon stride mbX mbY dqFactors lambda coeffProbs extAbove0 extAbove1 extAbove2 extAbove3 extLeft0 extLeft1 extLeft2 extLeft3 aNzY0 aNzY1 aNzY2 aNzY3 lNzY0 lNzY1 lNzY2 lNzY3 = do
  modesMut <- VSM.new 16 :: ST s (VSM.MVector s Word8)
  residuals <- VSM.new 16 :: ST s (VSM.MVector s Int16)
  nzGrid <- VSM.new 16 :: ST s (VSM.MVector s Word8)

  -- Start with the cost of signaling B_PRED in the Y mode tree
  let !bpredSignalCost = (lambda * bPredYModeCost) `div` 256

  let processSB !bi !totalCost
        | bi >= 16 = do
            modesVec <- VS.unsafeFreeze modesMut
            return (modesVec, totalCost)
        | otherwise = do
            let !subX = mbX + (bi .&. 3) * 4
                !subY = mbY + (bi `shiftR` 2) * 4
                !row = bi `shiftR` 2
                !col = bi .&. 3

            -- Determine above/left mode context for this sub-block
            !aboveMode <-
              if row == 0
                then return $ case col of 0 -> extAbove0; 1 -> extAbove1; 2 -> extAbove2; _ -> extAbove3
                else do
                  !m <- VSM.unsafeRead modesMut ((row - 1) * 4 + col)
                  return (fromIntegral m)
            !leftMode <-
              if col == 0
                then return $ case row of 0 -> extLeft0; 1 -> extLeft1; 2 -> extLeft2; _ -> extLeft3
                else do
                  !m <- VSM.unsafeRead modesMut (row * 4 + col - 1)
                  return (fromIntegral m)

            -- Compute NZ context from committed blocks
            !aboveNz <- if row == 0
              then return $ case col of
                0 -> aNzY0; 1 -> aNzY1; 2 -> aNzY2; _ -> aNzY3
              else fromIntegral <$> VSM.unsafeRead nzGrid (bi - 4)
            !leftNz <- if col == 0
              then return $ case row of
                0 -> lNzY0; 1 -> lNzY1; 2 -> lNzY2; _ -> lNzY3
              else fromIntegral <$> VSM.unsafeRead nzGrid (bi - 1)
            let !ctx = min 2 (aboveNz + leftNz)

            -- Try all 10 modes, pick best by RD cost
            let tryMode !m !bestMode !bestCost
                  | m > 9 = return (bestMode, bestCost)
                  | otherwise = do
                      -- Predict into yRecon (reads only from outside the 4x4 block)
                      predict4x4 m yRecon stride subX subY

                      -- Compute residuals
                      let fillRes !r
                            | r >= 4 = return ()
                            | otherwise = do
                                let fillCol !c
                                      | c >= 4 = fillRes (r + 1)
                                      | otherwise = do
                                          let !idx = (subY + r) * stride + (subX + c)
                                          !o <- VSM.unsafeRead yOrig idx
                                          !p <- VSM.unsafeRead yRecon idx
                                          VSM.unsafeWrite residuals (r * 4 + c) (fromIntegral o - fromIntegral p :: Int16)
                                          fillCol (c + 1)
                                fillCol 0
                      fillRes 0

                      fdct4x4 residuals
                      applySharpen dqFactors 3 residuals
                      _ <- trellisQuantizeBlock dqFactors 3 residuals coeffProbs ctx 0 lambda
                      !blockBitCost <- coeffBlockCost residuals coeffProbs 3 ctx 0
                      dequantizeBlock dqFactors 3 residuals
                      idct4x4 residuals
                      !sse <- computeBlockSSEM yOrig yRecon residuals stride subX subY

                      let !modeBitCost = bSubModeCost aboveMode leftMode m
                          !cost = sse + (lambda * (blockBitCost + modeBitCost)) `div` 256
                      if cost < bestCost
                        then tryMode (m + 1) m cost
                        else tryMode (m + 1) bestMode bestCost

            (!bestMode, !bestCost) <- tryMode 0 0 maxBound

            -- Commit: predict with best mode and reconstruct into yRecon
            predict4x4 bestMode yRecon stride subX subY
            let fillRes !r
                  | r >= 4 = return ()
                  | otherwise = do
                      let fillCol !c
                            | c >= 4 = fillRes (r + 1)
                            | otherwise = do
                                let !idx = (subY + r) * stride + (subX + c)
                                !o <- VSM.unsafeRead yOrig idx
                                !p <- VSM.unsafeRead yRecon idx
                                VSM.unsafeWrite residuals (r * 4 + c) (fromIntegral o - fromIntegral p :: Int16)
                                fillCol (c + 1)
                      fillCol 0
            fillRes 0
            fdct4x4 residuals
            applySharpen dqFactors 3 residuals
            !hasNz <- trellisQuantizeBlock dqFactors 3 residuals coeffProbs ctx 0 lambda
            dequantizeBlock dqFactors 3 residuals
            idct4x4 residuals

            -- Write reconstruction to yRecon
            let reconSB !r
                  | r >= 4 = return ()
                  | otherwise = do
                      let reconCol !c
                            | c >= 4 = reconSB (r + 1)
                            | otherwise = do
                                let !idx = (subY + r) * stride + (subX + c)
                                !p <- VSM.unsafeRead yRecon idx
                                !res <- VSM.unsafeRead residuals (r * 4 + c)
                                VSM.unsafeWrite yRecon idx (clip255 (fromIntegral p + fromIntegral res))
                                reconCol (c + 1)
                      reconCol 0
            reconSB 0

            VSM.unsafeWrite nzGrid bi (if hasNz then 1 else 0)
            VSM.unsafeWrite modesMut bi (fromIntegral bestMode)
            processSB (bi + 1) (totalCost + bestCost)

  processSB 0 bpredSignalCost

-- | Select best chroma mode using true RDO over both U and V planes.
-- Score = (SSE_U + SSE_V) + (lambda * (bitCost_U + bitCost_V + modeCost)) / 256.
{-# INLINE selectChromaModeRDO #-}
selectChromaModeRDO ::
  VSM.MVector s Word8 -> -- U original
  VSM.MVector s Word8 -> -- U reconstruction
  VSM.MVector s Word8 -> -- V original
  VSM.MVector s Word8 -> -- V reconstruction
  Int -> -- Stride
  Int -> -- X position (chroma coords)
  Int -> -- Y position (chroma coords)
  DequantFactors ->
  Int -> -- Lambda
  VU.Vector Word8 -> -- Coefficient probabilities (1056 flat entries)
  ST s (Int, Int) -- (mode, rdCost)
selectChromaModeRDO uOrig uRecon vOrig vRecon stride x y dqFactors lambda coeffProbs = do
  uPredBuf <- VSM.clone uRecon
  vPredBuf <- VSM.clone vRecon
  residuals <- VSM.new 16

  let tryMode !mode !bestMode !bestCost
        | mode > 3 = return (bestMode, bestCost)
        | otherwise = do
            predict8x8 mode uPredBuf stride x y
            predict8x8 mode vPredBuf stride x y
            (!sseU, !bitCostU) <- trialEncodeChroma8x8 uOrig uPredBuf residuals stride x y dqFactors lambda coeffProbs
            (!sseV, !bitCostV) <- trialEncodeChroma8x8 vOrig vPredBuf residuals stride x y dqFactors lambda coeffProbs
            let !modeBitCost = uvModeCost mode
                !rdCost = (sseU + sseV) + (lambda * (bitCostU + bitCostV + modeBitCost)) `div` 256
            if rdCost < bestCost
              then tryMode (mode + 1) mode rdCost
              else tryMode (mode + 1) bestMode bestCost

  tryMode 0 0 maxBound

-- | Trial-encode a single 8x8 chroma plane (4 blocks of 4x4).
-- Returns (SSE, bitCost) where bitCost is in 256ths of a bit.
{-# INLINE trialEncodeChroma8x8 #-}
trialEncodeChroma8x8 ::
  VSM.MVector s Word8 -> -- Chroma original
  VSM.MVector s Word8 -> -- Prediction buffer (prediction already applied)
  VSM.MVector s Int16 -> -- Reusable 16-element residual buffer
  Int -> -- Stride
  Int -> -- X position
  Int -> -- Y position
  DequantFactors ->
  Int -> -- Lambda for trellis quantization
  VU.Vector Word8 -> -- Coefficient probabilities
  ST s (Int, Int) -- (SSE, bitCost in 256ths)
trialEncodeChroma8x8 chromaOrig predBuf residuals stride x y dqFactors lambda coeffProbs = do
  let processBlock !bi !sse !rate
        | bi >= 4 = return (sse, rate)
        | otherwise = do
            let !row = bi `shiftR` 1
                !col = bi .&. 1
                !subX = col * 4
                !subY = row * 4
            let fillRes !r
                  | r >= 4 = return ()
                  | otherwise = do
                      let fillCol !c
                            | c >= 4 = fillRes (r + 1)
                            | otherwise = do
                                let !idx = (y + subY + r) * stride + (x + subX + c)
                                !o <- VSM.unsafeRead chromaOrig idx
                                !p <- VSM.unsafeRead predBuf idx
                                VSM.unsafeWrite residuals (r * 4 + c) (fromIntegral o - fromIntegral p :: Int16)
                                fillCol (c + 1)
                      fillCol 0
            fillRes 0
            fdct4x4 residuals
            _ <- trellisQuantizeBlock dqFactors 2 residuals coeffProbs 0 0 lambda
            !blockBitCost <- coeffBlockCost residuals coeffProbs 2 0 0
            dequantizeBlock dqFactors 2 residuals
            idct4x4 residuals
            !blockSSE <- computeBlockSSEM chromaOrig predBuf residuals stride (x + subX) (y + subY)
            processBlock (bi + 1) (sse + blockSSE) (rate + blockBitCost)
  processBlock 0 0 0

-- ---------------------------------------------------------------------------
-- RDO helpers
-- ---------------------------------------------------------------------------

-- | Compute SSE for one 4x4 block: original vs (prediction + IDCT residuals)
{-# INLINE computeBlockSSEM #-}
computeBlockSSEM ::
  VSM.MVector s Word8 -> -- Original pixels
  VSM.MVector s Word8 -> -- Prediction pixels
  VSM.MVector s Int16 -> -- IDCT output (reconstruction residuals)
  Int -> -- Stride
  Int -> -- X position
  Int -> -- Y position
  ST s Int
computeBlockSSEM orig predBuf idctOut stride x y = do
  let go !r !acc
        | r >= 4 = return acc
        | otherwise = do
            let goC !c !a
                  | c >= 4 = go (r + 1) a
                  | otherwise = do
                      let !idx = (y + r) * stride + (x + c)
                      !o <- VSM.unsafeRead orig idx
                      !p <- VSM.unsafeRead predBuf idx
                      !res <- VSM.unsafeRead idctOut (r * 4 + c)
                      let !recon = clip255 (fromIntegral p + fromIntegral res)
                          !diff = fromIntegral o - fromIntegral recon :: Int
                      goC (c + 1) (a + diff * diff)
            goC 0 acc
  go 0 0
