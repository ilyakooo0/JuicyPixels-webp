{-# LANGUAGE BangPatterns #-}

-- | VP8 coefficient probability statistics and optimization.
-- Collects branch counts during encoding, computes optimal probabilities,
-- and performs cost-benefit analysis to decide which probabilities to update.
module Codec.Picture.WebP.Internal.VP8.CoeffStats
  ( CoeffStats,
    newCoeffStats,
    recordBranch,
    computeOptimalProbs,
    decideUpdates,
  )
where

import Codec.Picture.WebP.Internal.VP8.RateCost (branchCost)
import Codec.Picture.WebP.Internal.VP8.Tables (coeffUpdateProbs, defaultCoeffProbs)
import Control.Monad.ST
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Mutable statistics accumulator for VP8 coefficient probability counting.
-- Tracks false/true branch counts at each of the 1056 probability positions
-- (4 block types × 8 bands × 3 contexts × 11 tokens).
data CoeffStats s = CoeffStats
  { csFalseCounts :: !(VSM.MVector s Word32),
    csTrueCounts :: !(VSM.MVector s Word32)
  }

-- | Allocate zero-initialized coefficient statistics.
newCoeffStats :: ST s (CoeffStats s)
newCoeffStats = do
  fc <- VSM.replicate 1056 0
  tc <- VSM.replicate 1056 0
  return (CoeffStats fc tc)

-- | Record a single branch decision at the given probability index.
{-# INLINE recordBranch #-}
recordBranch :: CoeffStats s -> Int -> Bool -> ST s ()
recordBranch !stats !idx !bit = do
  let !vec = if bit then csTrueCounts stats else csFalseCounts stats
  !old <- VSM.read vec idx
  VSM.write vec idx (old + 1)

-- | Compute optimal probabilities from branch statistics.
-- P(bit=0) = false_count / total, scaled to VP8 probability [1..255].
-- Falls back to default probability when no data is available.
computeOptimalProbs :: CoeffStats s -> ST s (VU.Vector Word8)
computeOptimalProbs stats =
  VU.generateM 1056 $ \i -> do
    fc <- VSM.read (csFalseCounts stats) i
    tc <- VSM.read (csTrueCounts stats) i
    let !total = fc + tc
    if total == 0
      then return (defaultCoeffProbs VU.! i)
      else do
        let !prob = (256 * fromIntegral fc) `div` fromIntegral total :: Word32
            !clamped = max 1 (min 255 prob)
        return (fromIntegral clamped :: Word8)

-- | Decide which probabilities to update based on cost-benefit analysis.
-- Returns (updated probs, update flags).
-- An update is signaled only when the entropy savings from using the new
-- probability exceed the header cost of writing the update.
decideUpdates :: CoeffStats s -> VU.Vector Word8 -> ST s (VU.Vector Word8, VU.Vector Bool)
decideUpdates stats optimalProbs = do
  pairs <- mapM decide [0 .. 1055]
  let !probs = VU.fromList (map fst pairs)
      !flags = VU.fromList (map snd pairs)
  return (probs, flags)
  where
    decide !i = do
      fcW <- VSM.read (csFalseCounts stats) i
      tcW <- VSM.read (csTrueCounts stats) i
      let !fc = fromIntegral fcW :: Int
          !tc = fromIntegral tcW :: Int
          !total = fc + tc
          !oldProb = defaultCoeffProbs VU.! i
          !newProb = optimalProbs VU.! i
      if total == 0 || oldProb == newProb
        then return (oldProb, False)
        else do
          let !updateProb = coeffUpdateProbs VU.! i
              -- Cost of encoding all branches with old vs new probability (256ths of a bit)
              !oldCost = fc * branchCost oldProb False + tc * branchCost oldProb True
              !newCost = fc * branchCost newProb False + tc * branchCost newProb True
              !savings = oldCost - newCost
              -- Overhead: True flag + 8-bit literal value, minus the False flag cost
              !overhead = branchCost updateProb True + 8 * 256 - branchCost updateProb False
          return $
            if savings > overhead
              then (newProb, True)
              else (oldProb, False)

