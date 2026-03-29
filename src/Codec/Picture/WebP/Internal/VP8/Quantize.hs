{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Picture.WebP.Internal.VP8.Quantize
  ( quantizeBlock,
    trellisQuantizeBlock,
    applySharpen,
    qualityToYacQi,
    rdLambdaFromQi,
  )
where

import Codec.Picture.WebP.Internal.VP8.Dequant
import Codec.Picture.WebP.Internal.VP8.RateCost (branchCost, trellisLevelCost)
import Codec.Picture.WebP.Internal.VP8.Tables (coeffBands, zigzag)
import Control.Monad.ST
import Data.Bits
import Data.Int
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- Performance: INLINE pragmas and manual loops with unsafeRead/unsafeWrite

-- | Convert quality parameter (0-100) to yac_qi value (0-127)
-- Higher quality → lower qi → finer quantization
--
-- Uses libwebp's non-linear mapping (QualityToCompression):
--   1. Piecewise linear: linear_c = q*(2/3) for q<0.75, else 2*q-1
--   2. Cube root: c = linear_c^(1/3)
--   3. qi = floor(127 * (1 - c))
--
-- This allocates more QI range to the perceptually-important high-quality
-- region (quality 75-100) and compresses the low-quality range.
qualityToYacQi :: Int -> Int
qualityToYacQi quality =
  let clamped = max 0 (min 100 quality)
      q = fromIntegral clamped / 100.0 :: Double
      linearC =
        if q < 0.75
          then q * (2.0 / 3.0)
          else 2.0 * q - 1.0
      c = linearC ** (1.0 / 3.0)
      qi = floor (127.0 * (1.0 - c))
   in max 0 (min 127 (qi :: Int))

-- | Quantize a 4x4 block in place
-- blockType: 0 = Y (AC only), 1 = Y2, 2 = UV, 3 = Y (full)
-- Input: DCT coefficients
-- Output: Quantized coefficients (in place)
{-# INLINE quantizeBlock #-}
quantizeBlock :: DequantFactors -> Int -> VSM.MVector s Int16 -> ST s ()
quantizeBlock factors blockType coeffs = do
  case blockType of
    0 -> quantYAC factors coeffs
    1 -> quantY2 factors coeffs
    2 -> quantUV factors coeffs
    3 -> quantYFull factors coeffs
    _ -> return ()

-- | Quantize Y block (AC only, position 0 is DC from Y2, don't quantize it here)
{-# INLINE quantYAC #-}
quantYAC :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantYAC factors coeffs = do
  let !quant = dqYAC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (quantizeCoeff c quant)
            go (i + 1)
  go 1

-- | Quantize Y2 DC block
{-# INLINE quantY2 #-}
quantY2 :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantY2 factors coeffs = do
  !c0 <- VSM.unsafeRead coeffs 0
  let !dcQuant = dqY2DC factors
  VSM.unsafeWrite coeffs 0 (quantizeCoeff c0 dcQuant)

  let !acQuant = dqY2AC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (quantizeCoeff c acQuant)
            go (i + 1)
  go 1

-- | Quantize UV block
{-# INLINE quantUV #-}
quantUV :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantUV factors coeffs = do
  !c0 <- VSM.unsafeRead coeffs 0
  let !dcQuant = dqUVDC factors
  VSM.unsafeWrite coeffs 0 (quantizeCoeff c0 dcQuant)

  let !acQuant = dqUVAC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (quantizeCoeff c acQuant)
            go (i + 1)
  go 1

-- | Quantize Y block (full, including DC at position 0)
{-# INLINE quantYFull #-}
quantYFull :: DequantFactors -> VSM.MVector s Int16 -> ST s ()
quantYFull factors coeffs = do
  !c0 <- VSM.unsafeRead coeffs 0
  let !dcQuant = dqYDC factors
  VSM.unsafeWrite coeffs 0 (quantizeCoeff c0 dcQuant)

  let !acQuant = dqYAC factors
  let go !i
        | i > 15 = return ()
        | otherwise = do
            !c <- VSM.unsafeRead coeffs i
            VSM.unsafeWrite coeffs i (quantizeCoeff c acQuant)
            go (i + 1)
  go 1

-- | Quantize a single coefficient
-- Formula: quantized = round(coeff / quant) = (abs(coeff) + quant/2) / quant
-- Preserves sign
{-# INLINE quantizeCoeff #-}
quantizeCoeff :: Int16 -> Int16 -> Int16
quantizeCoeff !coeff !quant
  | quant == 0 = 0 -- Avoid division by zero
  | otherwise =
      let absCoeff = abs (fromIntegral coeff :: Int)
          absQuant = abs (fromIntegral quant :: Int)
          -- Add rounding bias (quant / 2)
          quantized = (absCoeff + (absQuant `shiftR` 1)) `div` absQuant
          -- Preserve sign
          result = if coeff < 0 then -quantized else quantized
       in fromIntegral result

-- | Compute RDO lambda from quantizer index.
-- Controls the rate vs distortion tradeoff in mode selection:
--   low qi (high quality) → small lambda → prioritize distortion
--   high qi (low quality) → large lambda → prioritize rate
rdLambdaFromQi :: Int -> Int
rdLambdaFromQi qi = max 1 ((qi * qi + 8) `div` 16)

-- ---------------------------------------------------------------------------
-- Pre-quantization sharpening bias
-- ---------------------------------------------------------------------------

-- | Frequency-dependent sharpening weights (raster order, 4x4 block).
-- Higher-frequency positions get larger weights to preserve detail.
-- From libwebp kFreqSharpening[16].
kFreqSharpening :: VU.Vector Int
kFreqSharpening =
  VU.fromList [0, 30, 60, 90, 30, 60, 90, 90, 60, 90, 90, 90, 90, 90, 90, 90]

-- | Apply pre-quantization sharpening bias to DCT coefficients.
-- Adds a frequency-dependent bias proportional to the quantization step,
-- preserving high-frequency detail that would otherwise be zeroed out.
-- Only applied to Y blocks (types 0 and 3), not Y2 (type 1) or UV (type 2).
{-# INLINE applySharpen #-}
applySharpen :: DequantFactors -> Int -> VSM.MVector s Int16 -> ST s ()
applySharpen factors blockType coeffs
  | blockType == 0 || blockType == 3 = do
      let !q = fromIntegral (dqYAC factors) :: Int
      let go !i
            | i > 15 = return ()
            | otherwise = do
                let !s = kFreqSharpening VU.! i
                    !sharpen = fromIntegral ((s * q) `shiftR` 11) :: Int16
                !c <- VSM.unsafeRead coeffs i
                let !c' = if c < 0 then c - sharpen else c + sharpen
                VSM.unsafeWrite coeffs i c'
                go (i + 1)
      go 0
  | otherwise = return ()

-- ---------------------------------------------------------------------------
-- Trellis quantization
-- ---------------------------------------------------------------------------

-- | Trellis-optimized quantization for a 4x4 block.
-- Uses Viterbi dynamic programming to find the quantized coefficient levels
-- that minimize (lambda * rate + 256 * distortion), accounting for the VP8
-- coefficient entropy context that flows between positions.
--
-- At each scan position, considers two candidates: floor(|c|/Q) and
-- floor(|c|/Q)+1. The forward pass finds optimal predecessor chains;
-- backtracking writes the best signed levels to the coefficients buffer.
--
-- Returns True if any coefficient is nonzero after optimization.
{-# INLINE trellisQuantizeBlock #-}
trellisQuantizeBlock ::
  forall s.
  DequantFactors ->
  Int -> -- blockType (0=Y-AC, 1=Y2, 2=UV, 3=Y-full)
  VSM.MVector s Int16 -> -- coefficients (modified in place to quantized levels)
  VU.Vector Word8 -> -- coefficient probabilities (1056 entries)
  Int -> -- initial context (0, 1, or 2)
  Int -> -- start position (0 or 1)
  Int -> -- RDO lambda (same as mode-selection lambda)
  ST s Bool
trellisQuantizeBlock !factors !blockType !coeffs !coeffProbs !initialCtx !startPos !lambda = do
  let -- Quantization steps per position
      !qDC = fromIntegral (case blockType of
        1 -> dqY2DC factors; 2 -> dqUVDC factors; 3 -> dqYDC factors; _ -> dqYDC factors) :: Int
      !qAC = fromIntegral (case blockType of
        0 -> dqYAC factors; 1 -> dqY2AC factors; 2 -> dqUVAC factors; _ -> dqYAC factors) :: Int
      !lam = fromIntegral lambda :: Int64
      -- Sentinel for dead trellis nodes (large but won't overflow on addition)
      !dead = maxBound `div` 4 :: Int64

  -- Phase 1: Find last scan position where a nonzero quantized level is plausible
  let findLast !pos
        | pos < startPos = return (-1 :: Int)
        | otherwise = do
            let !zi = zigzag VU.! pos
                !q = if pos == 0 && blockType /= 0 then qDC else qAC
            !c <- fromIntegral <$> VSM.unsafeRead coeffs zi :: ST s Int
            if abs c * 2 >= q
              then return pos
              else findLast (pos - 1)

  lastPos <- findLast 15

  if lastPos < startPos
    then do
      -- All coefficients below threshold: zero them out
      let zeroAll !p
            | p > 15 = return ()
            | otherwise = do
                VSM.unsafeWrite coeffs (zigzag VU.! p) 0
                zeroAll (p + 1)
      zeroAll startPos
      return False
    else do
      -- Phase 2: Allocate trellis backtracking arrays
      -- prevCand[pos*2+cand] = which predecessor candidate (0 or 1) was best
      prevCand <- VUM.replicate 32 (0 :: Int)
      -- level0[pos] = truncated quantization level at each position
      level0Arr <- VUM.replicate 16 (0 :: Int)
      -- sign[pos] = sign of original coefficient (-1 or 1)
      signArr <- VUM.replicate 16 (1 :: Int16)

      -- Skip score: immediate EOB at startPos (baseline = all-zero block)
      let !startBand = coeffBands VU.! startPos
          !startPI = blockType * 264 + startBand * 33 + initialCtx * 11
          !skipScore = lam * fromIntegral (branchCost (coeffProbs VU.! startPI) False)

      -- Initial predecessor: virtual node before startPos.
      -- When initialCtx=0, the not-EOB cost at startPos is omitted by trellisLevelCost,
      -- so we must add it to the starting score. For initialCtx>0 it's already included.
      let !initScore = if initialCtx == 0
            then lam * fromIntegral (branchCost (coeffProbs VU.! startPI) True)
            else 0 :: Int64

      -- Phase 3: Forward Viterbi pass
      -- State: (score, context) for each of 2 candidates at previous position.
      -- Initially only one virtual predecessor is alive; the second is dead.
      let fwd !pos !ps0 !pc0 !ps1 !pc1 !bestS !bestP !bestC
            | pos > lastPos = return (bestS, bestP, bestC)
            | otherwise = do
                let !zi = zigzag VU.! pos
                !rawC <- fromIntegral <$> VSM.unsafeRead coeffs zi :: ST s Int
                let !ac = abs rawC
                    !sgn = if rawC < 0 then -1 else 1 :: Int16
                    !q = if pos == 0 && blockType /= 0 then qDC else qAC
                    !l0 = if q == 0 then 0 else ac `div` q
                    !band = coeffBands VU.! pos

                VUM.unsafeWrite level0Arr pos l0
                VUM.unsafeWrite signArr pos sgn

                -- Evaluate a candidate quantization level
                let {-# INLINE evalCand #-}
                    evalCand !lev =
                      let -- Distortion delta relative to all-zero baseline
                          !errI = fromIntegral (ac - lev * q) :: Int64
                          !acI = fromIntegral ac :: Int64
                          !dd = 256 * (errI * errI - acI * acI)
                          -- Rate cost from predecessor 0
                          !pi0 = blockType * 264 + band * 33 + pc0 * 11
                          !r0 = trellisLevelCost coeffProbs pi0 pc0 lev
                          !s0 = if ps0 >= dead then dead else ps0 + lam * fromIntegral r0
                          -- Rate cost from predecessor 1
                          !pi1 = blockType * 264 + band * 33 + pc1 * 11
                          !r1 = trellisLevelCost coeffProbs pi1 pc1 lev
                          !s1 = if ps1 >= dead then dead else ps1 + lam * fromIntegral r1
                          -- Pick best predecessor
                          (!bestPrevS, !bestPrevI) = if s0 <= s1 then (s0, 0) else (s1, 1)
                          -- Node score = best predecessor + distortion delta
                          !nodeS = bestPrevS + dd
                          -- Context for next position
                          !newCtx = if lev == 0 then 0 else if lev == 1 then 1 else 2
                       in (nodeS, newCtx, bestPrevI)

                -- Candidate 0: level = l0 (truncation toward zero)
                let (!cs0, !cc0, !cp0) = evalCand l0
                VUM.unsafeWrite prevCand (pos * 2) cp0

                -- Candidate 1: level = l0 + 1 (one step above truncation)
                let (!cs1, !cc1, !cp1) = evalCand (l0 + 1)
                VUM.unsafeWrite prevCand (pos * 2 + 1) cp1

                -- Check if either candidate is a valid terminal (last nonzero)
                let {-# INLINE checkTerm #-}
                    checkTerm !score !ctx !cIdx !bS !bP !bC
                      | score >= dead = (bS, bP, bC)
                      | l0 + cIdx == 0 = (bS, bP, bC) -- zero level: not a terminal
                      | otherwise =
                          let !np = pos + 1
                              !ec =
                                if np >= 16
                                  then 0
                                  else
                                    let !nb = coeffBands VU.! np
                                        !npi = blockType * 264 + nb * 33 + ctx * 11
                                     in branchCost (coeffProbs VU.! npi) False
                              !ts = score + lam * fromIntegral ec
                           in if ts < bS then (ts, pos, cIdx) else (bS, bP, bC)

                let (!bS1, !bP1, !bC1) = checkTerm cs0 cc0 0 bestS bestP bestC
                    (!bS2, !bP2, !bC2) = checkTerm cs1 cc1 1 bS1 bP1 bC1

                fwd (pos + 1) cs0 cc0 cs1 cc1 bS2 bP2 bC2

      (!finalBS, !finalBP, !finalBC) <-
        fwd startPos initScore initialCtx dead 0 skipScore (-1) (-1)

      -- Phase 4: Write results
      if finalBP < startPos || finalBS >= dead
        then do
          -- Skip won: zero all coefficients
          let zeroAll !p
                | p > 15 = return ()
                | otherwise = do
                    VSM.unsafeWrite coeffs (zigzag VU.! p) 0
                    zeroAll (p + 1)
          zeroAll startPos
          return False
        else do
          -- Zero positions after the terminal
          let zeroAfter !p
                | p > 15 = return ()
                | otherwise = do
                    VSM.unsafeWrite coeffs (zigzag VU.! p) 0
                    zeroAfter (p + 1)
          zeroAfter (finalBP + 1)

          -- Backtrack from (finalBP, finalBC) to startPos, writing quantized levels
          let backtrack !pos !cand
                | pos < startPos = return ()
                | otherwise = do
                    !l0 <- VUM.unsafeRead level0Arr pos
                    !sgn <- VUM.unsafeRead signArr pos
                    let !level = min 2047 (l0 + cand)
                        !quantized = sgn * fromIntegral level
                    VSM.unsafeWrite coeffs (zigzag VU.! pos) quantized
                    !pCand <- VUM.unsafeRead prevCand (pos * 2 + cand)
                    backtrack (pos - 1) pCand

          backtrack finalBP finalBC
          return True
