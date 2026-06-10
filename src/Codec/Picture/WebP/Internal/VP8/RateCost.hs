{-# LANGUAGE BangPatterns #-}

-- | Rate estimation for VP8 Rate-Distortion Optimization.
-- Provides bit-cost functions that mirror the actual encoding logic
-- (coefficient token trees, mode signaling) without producing output bytes.
-- All costs are in 256ths of a bit (matching VP8 arithmetic coder theory).
module Codec.Picture.WebP.Internal.VP8.RateCost
  ( branchCost,
    branchCostTab,
    coeffBlockCost,
    trellisLevelCost,
    i16ModeCost,
    bSubModeCost,
    uvModeCost,
    bPredYModeCost,
  )
where

import Codec.Picture.WebP.Internal.VP8.Tables
  ( coeffBands,
    kfBmodeProbs,
    pcatProbs,
    zigzag,
  )
import Control.Monad.ST
import Data.Bits (testBit)
import Data.Int (Int16)
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- ---------------------------------------------------------------------------
-- Branch cost
-- ---------------------------------------------------------------------------

-- | Cost of coding a bit with a given probability, in 256ths of a bit.
-- VP8 probability p means P(bit=False) = p/256.
{-# INLINE branchCost #-}
branchCost :: Word8 -> Bool -> Int
branchCost !prob !bit =
  let !p = if bit then 256 - fromIntegral prob else fromIntegral prob :: Int
      !idx = max 0 (min 255 p)
   in branchCostTab VU.! idx

-- | Precomputed branch cost lookup table.
-- branchCostTab[p] = round(256 * -log2(p/256)) for p in 0..255.
branchCostTab :: VU.Vector Int
branchCostTab =
  VU.generate 256 $ \p ->
    if p <= 0
      then 2048
      else min 2048 $ round (256.0 * negate (logBase 2.0 (fromIntegral p / 256.0 :: Double)) :: Double)

-- ---------------------------------------------------------------------------
-- Trellis per-coefficient cost
-- ---------------------------------------------------------------------------

-- | Cost of encoding a single quantized level for trellis optimization.
-- probIdx must be pre-computed as: blockType * 264 + band * 33 + ctx * 11.
-- Handles the skipEOB convention: ctx > 0 includes not-EOB cost; ctx = 0 omits it
-- (caller must add it separately for the first position when initialCtx = 0).
-- Includes sign bit cost (256 = 1 bit) for nonzero levels.
{-# INLINE trellisLevelCost #-}
trellisLevelCost :: VU.Vector Word8 -> Int -> Int -> Int -> Int
trellisLevelCost !coeffProbs !probIdx !ctx !level
  | level == 0 =
      let !notEob = if ctx > 0 then branchCost (coeffProbs VU.! probIdx) True else 0
          !zero = branchCost (coeffProbs VU.! (probIdx + 1)) False
       in notEob + zero
  | otherwise =
      let !notEob = if ctx > 0 then branchCost (coeffProbs VU.! probIdx) True else 0
          !nz = branchCost (coeffProbs VU.! (probIdx + 1)) True
          !val =
            valueBitCost
              (coeffProbs VU.! (probIdx + 2))
              (coeffProbs VU.! (probIdx + 3))
              (coeffProbs VU.! (probIdx + 4))
              (coeffProbs VU.! (probIdx + 5))
              (coeffProbs VU.! (probIdx + 6))
              (coeffProbs VU.! (probIdx + 7))
              (coeffProbs VU.! (probIdx + 8))
              (coeffProbs VU.! (probIdx + 9))
              (coeffProbs VU.! (probIdx + 10))
              level
       in notEob + nz + val

-- ---------------------------------------------------------------------------
-- Coefficient block cost
-- ---------------------------------------------------------------------------

-- | Estimate the bit cost of encoding a 4x4 block's quantized coefficients.
-- Mirrors encodeCoefficients exactly: same zigzag order, probability lookup,
-- EOB/zero/nonzero branching, and value encoding tree.
-- Returns cost in 256ths of a bit.
{-# INLINE coeffBlockCost #-}
coeffBlockCost ::
  VSM.MVector s Int16 -> -- Quantized coefficients (raster scan order from FDCT)
  VU.Vector Word8 -> -- Coefficient probabilities (flat: 4*8*3*11 = 1056 entries)
  Int -> -- Block type (0=Y-AC, 1=Y2, 2=chroma, 3=Y-full)
  Int -> -- Initial context (0, 1, or 2)
  Int -> -- Start position (0 or 1)
  ST s Int -- Cost in 256ths of a bit
coeffBlockCost coeffs coeffProbs blockType initialCtx startPos = do
  lastNzPos <- findLastNonzero coeffs startPos
  case lastNzPos of
    Nothing -> do
      -- All zeros: EOB
      let !band = coeffBands VU.! startPos
          !probIdx = blockType * 264 + band * 33 + initialCtx * 11
      return $! branchCost (coeffProbs VU.! probIdx) False
    Just lastNz -> do
      let loop !pos !ctx !acc !skipEOB
            | pos > lastNz =
                if pos >= 16
                  then return acc
                  else
                    if skipEOB
                      then return acc -- Unreachable (lastNz is always nonzero)
                      else do
                        let !band = coeffBands VU.! pos
                            !probIdx = blockType * 264 + band * 33 + ctx * 11
                        return $! acc + branchCost (coeffProbs VU.! probIdx) False
            | otherwise = do
                let !zigzagIdx = zigzag VU.! pos
                !coeff <- VSM.unsafeRead coeffs zigzagIdx
                let !band = coeffBands VU.! pos
                    !probIdx = blockType * 264 + band * 33 + ctx * 11
                if coeff == 0
                  then do
                    let !cost =
                          if skipEOB
                            then branchCost (coeffProbs VU.! (probIdx + 1)) False
                            else
                              branchCost (coeffProbs VU.! probIdx) True
                                + branchCost (coeffProbs VU.! (probIdx + 1)) False
                    loop (pos + 1) 0 (acc + cost) True
                  else do
                    let !absCoeff = abs (fromIntegral coeff :: Int)
                        !eobCost =
                          if skipEOB
                            then 0
                            else branchCost (coeffProbs VU.! probIdx) True
                        !nzCost = branchCost (coeffProbs VU.! (probIdx + 1)) True
                        !valCost =
                          valueBitCost
                            (coeffProbs VU.! (probIdx + 2))
                            (coeffProbs VU.! (probIdx + 3))
                            (coeffProbs VU.! (probIdx + 4))
                            (coeffProbs VU.! (probIdx + 5))
                            (coeffProbs VU.! (probIdx + 6))
                            (coeffProbs VU.! (probIdx + 7))
                            (coeffProbs VU.! (probIdx + 8))
                            (coeffProbs VU.! (probIdx + 9))
                            (coeffProbs VU.! (probIdx + 10))
                            absCoeff
                        !newCtx = if absCoeff == 1 then 1 else 2
                    loop (pos + 1) newCtx (acc + eobCost + nzCost + valCost) False
      loop startPos initialCtx 0 False
  where
    findLastNonzero cs start = go Nothing start
      where
        go lastFound pos
          | pos >= 16 = return lastFound
          | otherwise = do
              let !zigzagIdx = zigzag VU.! pos
              !coeff <- VSM.unsafeRead cs zigzagIdx
              let !newLast = if coeff /= 0 then Just pos else lastFound
              go newLast (pos + 1)

-- | Bit cost of encoding a coefficient value (tree decisions + extra bits + sign).
-- Mirrors encodeValue from EncodeCoefficients.hs exactly.
-- Includes the sign bit cost (always 256 = 1 bit at probability 128).
{-# INLINE valueBitCost #-}
valueBitCost ::
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 -> -- p[2], p[3], p[4], p[5]
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 -> -- p[6], p[7], p[8], p[9], p[10]
  Int ->
  -- | coeff|
  Int -- Cost in 256ths of a bit
valueBitCost !p2 !p3 !p4 !p5 !p6 !p7 !p8 !p9 !p10 !absVal
  | absVal == 1 =
      branchCost p2 False + 256
  | absVal == 2 =
      branchCost p2 True + branchCost p3 False + branchCost p4 False + 256
  | absVal == 3 =
      branchCost p2 True + branchCost p3 False + branchCost p4 True + branchCost p5 False + 256
  | absVal == 4 =
      branchCost p2 True + branchCost p3 False + branchCost p4 True + branchCost p5 True + 256
  | absVal <= 6 =
      -- CAT1 (5-6)
      branchCost p2 True
        + branchCost p3 True
        + branchCost p6 False
        + branchCost p7 False
        + catExtraCost 0 (absVal - 5)
        + 256
  | absVal <= 10 =
      -- CAT2 (7-10)
      branchCost p2 True
        + branchCost p3 True
        + branchCost p6 False
        + branchCost p7 True
        + catExtraCost 1 (absVal - 7)
        + 256
  | absVal <= 18 =
      -- CAT3 (11-18)
      branchCost p2 True
        + branchCost p3 True
        + branchCost p6 True
        + branchCost p8 False
        + branchCost p9 False
        + catExtraCost 2 (absVal - 11)
        + 256
  | absVal <= 34 =
      -- CAT4 (19-34)
      branchCost p2 True
        + branchCost p3 True
        + branchCost p6 True
        + branchCost p8 False
        + branchCost p9 True
        + catExtraCost 3 (absVal - 19)
        + 256
  | absVal <= 66 =
      -- CAT5 (35-66)
      branchCost p2 True
        + branchCost p3 True
        + branchCost p6 True
        + branchCost p8 True
        + branchCost p10 False
        + catExtraCost 4 (absVal - 35)
        + 256
  | otherwise =
      -- CAT6 (67-2048)
      branchCost p2 True
        + branchCost p3 True
        + branchCost p6 True
        + branchCost p8 True
        + branchCost p10 True
        + catExtraCost 5 (absVal - 67)
        + 256

-- | Cost of encoding category extra bits (fixed probabilities from pcatProbs).
-- Bits are encoded MSB-first with per-bit probabilities.
{-# INLINE catExtraCost #-}
catExtraCost :: Int -> Int -> Int
catExtraCost !cat !extra =
  let probs = pcatProbs V.! cat
      nBits = VU.length probs - 1 -- -1 for trailing 0 sentinel
      go !i !acc
        | i >= nBits = acc
        | otherwise =
            let !p = probs VU.! i
                !bitPos = nBits - 1 - i
                !bit = testBit extra bitPos
             in go (i + 1) (acc + branchCost p bit)
   in go 0 0

-- ---------------------------------------------------------------------------
-- Mode costs
-- ---------------------------------------------------------------------------

-- | Cost of signaling I16 Y mode (DC/V/H/TM), in 256ths of a bit.
-- Tree: B_PRED="0", DC="100", V="101", H="110", TM="111"
-- Probs: [145, 156, 163, 128]
{-# INLINE i16ModeCost #-}
i16ModeCost :: Int -> Int
i16ModeCost 0 = branchCost 145 True + branchCost 156 False + branchCost 163 False -- DC
i16ModeCost 1 = branchCost 145 True + branchCost 156 False + branchCost 163 True -- V
i16ModeCost 2 = branchCost 145 True + branchCost 156 True + branchCost 128 False -- H
i16ModeCost 3 = branchCost 145 True + branchCost 156 True + branchCost 128 True -- TM
i16ModeCost _ = 0

-- | Cost of signaling B_PRED in the Y mode tree (code "0", prob 145).
{-# INLINE bPredYModeCost #-}
bPredYModeCost :: Int
bPredYModeCost = branchCost 145 False

-- | Cost of signaling a 4x4 sub-block intra mode, in 256ths of a bit.
-- Context-dependent via kfBmodeProbs[above*90 + left*9 + nodeIdx].
-- Tree structure mirrors encodeBSubMode from EncodeMode.hs.
{-# INLINE bSubModeCost #-}
bSubModeCost :: Int -> Int -> Int -> Int
bSubModeCost !aboveMode !leftMode !subMode =
  let !probBase = aboveMode * 90 + leftMode * 9
      !p0 = kfBmodeProbs VU.! probBase
      !p1 = kfBmodeProbs VU.! (probBase + 1)
      !p2 = kfBmodeProbs VU.! (probBase + 2)
      !p3 = kfBmodeProbs VU.! (probBase + 3)
      !p4 = kfBmodeProbs VU.! (probBase + 4)
      !p5 = kfBmodeProbs VU.! (probBase + 5)
      !p6 = kfBmodeProbs VU.! (probBase + 6)
      !p7 = kfBmodeProbs VU.! (probBase + 7)
      !p8 = kfBmodeProbs VU.! (probBase + 8)
   in case subMode of
        0 -> branchCost p0 False -- B_DC
        1 -> branchCost p0 True + branchCost p1 False -- B_TM
        2 -> branchCost p0 True + branchCost p1 True + branchCost p2 False -- B_VE
        3 ->
          branchCost p0 True
            + branchCost p1 True
            + branchCost p2 True
            + branchCost p3 False
            + branchCost p4 False -- B_HE
        4 ->
          branchCost p0 True
            + branchCost p1 True
            + branchCost p2 True
            + branchCost p3 True
            + branchCost p6 False -- B_LD
        5 ->
          branchCost p0 True
            + branchCost p1 True
            + branchCost p2 True
            + branchCost p3 False
            + branchCost p4 True
            + branchCost p5 False -- B_RD
        6 ->
          branchCost p0 True
            + branchCost p1 True
            + branchCost p2 True
            + branchCost p3 False
            + branchCost p4 True
            + branchCost p5 True -- B_VR
        7 ->
          branchCost p0 True
            + branchCost p1 True
            + branchCost p2 True
            + branchCost p3 True
            + branchCost p6 True
            + branchCost p7 False -- B_VL
        8 ->
          branchCost p0 True
            + branchCost p1 True
            + branchCost p2 True
            + branchCost p3 True
            + branchCost p6 True
            + branchCost p7 True
            + branchCost p8 False -- B_HD
        9 ->
          branchCost p0 True
            + branchCost p1 True
            + branchCost p2 True
            + branchCost p3 True
            + branchCost p6 True
            + branchCost p7 True
            + branchCost p8 True -- B_HU
        _ -> 0

-- | Cost of signaling UV mode (DC/V/H/TM), in 256ths of a bit.
-- Tree: DC="0", V="10", H="110", TM="111"
-- Probs: [142, 114, 183]
{-# INLINE uvModeCost #-}
uvModeCost :: Int -> Int
uvModeCost 0 = branchCost 142 False -- DC
uvModeCost 1 = branchCost 142 True + branchCost 114 False -- V
uvModeCost 2 = branchCost 142 True + branchCost 114 True + branchCost 183 False -- H
uvModeCost 3 = branchCost 142 True + branchCost 114 True + branchCost 183 True -- TM
uvModeCost _ = 0
