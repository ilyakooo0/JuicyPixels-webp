{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.EncodeCoefficients
  ( encodeCoefficients,
    countCoefficients,
  )
where

import Codec.Picture.WebP.Internal.VP8.BoolEncoder
import Codec.Picture.WebP.Internal.VP8.CoeffStats (CoeffStats, recordBranch)
import Codec.Picture.WebP.Internal.VP8.Tables
import Control.Monad.ST
import Data.Bits
import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Encode DCT coefficients for a 4x4 block using flat probability indexing.
-- This matches libwebp's GetCoeffsFast exactly: p[0]=EOB, p[1]=zero,
-- p[2]=is_one, p[3..5]=small values (2-4), p[6..10]=large values (CAT1-6).
-- Returns: (updated encoder, has_nonzero)
encodeCoefficients ::
  VSM.MVector s Int16 -> -- Quantized coefficients (in raster scan order from FDCT)
  VU.Vector Word8 -> -- Coefficient probabilities (flat: 4*8*3*11 = 1056 entries)
  Int -> -- Block type (libwebp: 0=i16-AC/Y-AC, 1=i16-DC/Y2, 2=chroma, 3=i4-AC)
  Int -> -- Initial context (0, 1, or 2)
  Int -> -- Start position (0 or 1)
  BoolEncoder ->
  ST s (BoolEncoder, Bool)
encodeCoefficients coeffs coeffProbs blockType initialCtx startPos encoder = do
  -- First, find the last nonzero coefficient position
  lastNzPos <- findLastNonzero coeffs startPos

  case lastNzPos of
    Nothing -> do
      -- All zeros: write EOB (p[0] = False)
      let band = coeffBands VU.! startPos
          probIdx = blockType * 264 + band * 33 + initialCtx * 11
          enc' = boolWrite (coeffProbs VU.! probIdx) False encoder -- p[0] = False → EOB
      return (enc', False)
    Just lastNz -> do
      -- There are nonzeros: encode coefficients up to and including lastNz, then EOB
      -- skipEOB: after DCT_0, skip p[0] (EOB check) at next position
      let loop !pos !ctx !enc !skipEOB
            | pos > lastNz = do
                -- After the last nonzero, write EOB
                if pos >= 16
                  then return (enc, True) -- No EOB needed at position 16
                  else do
                    let band = coeffBands VU.! pos
                        probIdx = blockType * 264 + band * 33 + ctx * 11
                    if skipEOB
                      then
                        -- After DCT_0: decoder starts at p[1] not p[0]
                        -- For EOB after skipEOB: don't write p[0], the decoder
                        -- will read p[1] and we need False to indicate zero, then
                        -- it will loop and check p[0] for EOB.
                        -- Actually: in libwebp, after zero, the inner while loop reads p[1].
                        -- If p[1]=False (zero again), it advances and checks p[0] at the new position.
                        -- But we want EOB at this position with skipEOB.
                        -- With skipEOB, the decoder is inside the while(!p[1]) loop.
                        -- It reads p[1]: if False, advances to next pos. If True, reads value.
                        -- We can't signal EOB from inside the while loop!
                        -- Actually: after the while loop ends (p[1]=True), the decoder reads p[2].
                        -- We can't go back to EOB from that point either.
                        --
                        -- The correct approach: we shouldn't be here with skipEOB for EOB.
                        -- When the last nonzero is followed by trailing zeros, we encode
                        -- those trailing zeros as part of the stream, then EOB.
                        -- But we DON'T encode trailing zeros — we stop at lastNz and write EOB.
                        --
                        -- Re-think: in the standard, after DCT_0, the decoder does skip EOB.
                        -- But if we're writing EOB here, it means we had a nonzero followed by
                        -- a zero (skipEOB=True), then we're at pos > lastNz, meaning we need EOB.
                        -- With skipEOB, the decoder would read p[1]. We need to signal "not more data".
                        -- The way to do this is: the bits after DCT_0 are at coeff_tree+2.
                        -- The only way to reach EOB from coeff_tree+2 is impossible — EOB is at root.
                        -- So after DCT_0, you CAN'T have an immediate EOB.
                        -- This means we must pad with explicit zeros until EOB can be written.
                        --
                        -- Actually, this CAN'T happen in practice: if skipEOB is True, the previous
                        -- token was DCT_0, which means the previous coeff was 0. But we only encode
                        -- up to lastNz, and after lastNz all coeffs are zero. If lastNz was followed
                        -- by a zero at lastNz+1 with skipEOB... wait, we never encode past lastNz.
                        --
                        -- Let me re-examine: the loop goes pos=startPos..lastNz encoding each coeff.
                        -- At pos=lastNz, the coeff is nonzero, so skipEOB becomes False.
                        -- Then pos=lastNz+1 > lastNz, and skipEOB is False. So this branch is never taken!
                        --
                        -- But what if lastNz's coeff is zero? findLastNonzero only sets lastNz
                        -- for nonzero positions, so lastNz's coeff is always nonzero. So skipEOB
                        -- is always False when we enter the EOB branch. Good.
                        return (enc, True) -- Unreachable, but safe fallback
                      else do
                        -- Normal EOB: write p[0] = False
                        let enc' = boolWrite (coeffProbs VU.! probIdx) False enc
                        return (enc', True)
            | otherwise = do
                let zigzagIdx = zigzag VU.! pos
                coeff <- VSM.read coeffs zigzagIdx
                let band = coeffBands VU.! pos
                    probIdx = blockType * 264 + band * 33 + ctx * 11

                if coeff == 0
                  then do
                    -- DCT_0: p[0]=True (not EOB), p[1]=False (zero)
                    let enc' =
                          if skipEOB
                            then boolWrite (coeffProbs VU.! (probIdx + 1)) False enc -- p[1]=False
                            else
                              let e1 = boolWrite (coeffProbs VU.! probIdx) True enc -- p[0]=True
                               in boolWrite (coeffProbs VU.! (probIdx + 1)) False e1 -- p[1]=False
                    loop (pos + 1) 0 enc' True
                  else do
                    -- Nonzero coefficient: encode token using flat p[k] indices
                    let absCoeff = abs (fromIntegral coeff :: Int)
                        enc1 =
                          if skipEOB
                            then enc -- Skip p[0]
                            else boolWrite (coeffProbs VU.! probIdx) True enc -- p[0]=True (not EOB)
                        enc2 = boolWrite (coeffProbs VU.! (probIdx + 1)) True enc1 -- p[1]=True (nonzero)
                        enc3 =
                          encodeValue
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
                            coeff
                            enc2
                        newCtx = if absCoeff == 1 then 1 else 2
                    loop (pos + 1) newCtx enc3 False

      loop startPos initialCtx encoder False
  where
    findLastNonzero cs start = go Nothing start
      where
        go lastFound pos
          | pos >= 16 = return lastFound
          | otherwise = do
              let zigzagIdx = zigzag VU.! pos
              coeff <- VSM.read cs zigzagIdx
              let newLast = if coeff /= 0 then Just pos else lastFound
              go newLast (pos + 1)

-- | Encode a coefficient value using flat probability indexing matching libwebp.
-- p2..p10 are the coefficient probabilities at indices [2..10].
-- This matches GetCoeffsFast + GetLargeValue from libwebp exactly.
{-# INLINE encodeValue #-}
encodeValue ::
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
  Int16 -> -- coeff (signed, for sign bit)
  BoolEncoder ->
  BoolEncoder
encodeValue !p2 !p3 !p4 !p5 !p6 !p7 !p8 !p9 !p10 !absVal !coeff !enc
  | absVal == 1 =
      -- p[2] = False (value is 1)
      let e1 = boolWrite p2 False enc
       in boolWrite 128 (coeff < 0) e1
  | absVal == 2 =
      -- p[2]=True, p[3]=False (small), p[4]=False (value 2)
      let e1 = boolWrite p2 True enc
          e2 = boolWrite p3 False e1
          e3 = boolWrite p4 False e2
       in boolWrite 128 (coeff < 0) e3
  | absVal == 3 =
      -- p[2]=True, p[3]=False, p[4]=True, p[5]=False (value 3)
      let e1 = boolWrite p2 True enc
          e2 = boolWrite p3 False e1
          e3 = boolWrite p4 True e2
          e4 = boolWrite p5 False e3
       in boolWrite 128 (coeff < 0) e4
  | absVal == 4 =
      -- p[2]=True, p[3]=False, p[4]=True, p[5]=True (value 4)
      let e1 = boolWrite p2 True enc
          e2 = boolWrite p3 False e1
          e3 = boolWrite p4 True e2
          e4 = boolWrite p5 True e3
       in boolWrite 128 (coeff < 0) e4
  | absVal <= 6 =
      -- CAT1 (5-6): p[2]=T, p[3]=T, p[6]=F, p[7]=F/T
      let e1 = boolWrite p2 True enc
          e2 = boolWrite p3 True e1
          e3 = boolWrite p6 False e2
          e4 = boolWrite p7 False e3
          -- Extra bit: 5 → bit=False, 6 → bit=True
          probs = pcatProbs V.! 0
          e5 = boolWrite (probs VU.! 0) (absVal == 6) e4
       in boolWrite 128 (coeff < 0) e5
  | absVal <= 10 =
      -- CAT2 (7-10): p[2]=T, p[3]=T, p[6]=F, p[7]=T
      let e1 = boolWrite p2 True enc
          e2 = boolWrite p3 True e1
          e3 = boolWrite p6 False e2
          e4 = boolWrite p7 True e3
          -- Extra bits: value - 7 as 2-bit MSB-first
          extra = absVal - 7
          probs = pcatProbs V.! 1
          e5 = boolWrite (probs VU.! 0) (testBit extra 1) e4
          e6 = boolWrite (probs VU.! 1) (testBit extra 0) e5
       in boolWrite 128 (coeff < 0) e6
  | absVal <= 18 =
      -- CAT3 (11-18): p[2]=T, p[3]=T, p[6]=T, p[8]=F, p[9]=F
      let e1 = boolWrite p2 True enc
          e2 = boolWrite p3 True e1
          e3 = boolWrite p6 True e2
          e4 = boolWrite p8 False e3
          e5 = boolWrite p9 False e4
          -- Extra bits: value - 11 as 3-bit MSB-first
          extra = absVal - 11
          probs = pcatProbs V.! 2
          e6 = boolWrite (probs VU.! 0) (testBit extra 2) e5
          e7 = boolWrite (probs VU.! 1) (testBit extra 1) e6
          e8 = boolWrite (probs VU.! 2) (testBit extra 0) e7
       in boolWrite 128 (coeff < 0) e8
  | absVal <= 34 =
      -- CAT4 (19-34): p[2]=T, p[3]=T, p[6]=T, p[8]=F, p[9]=T
      let e1 = boolWrite p2 True enc
          e2 = boolWrite p3 True e1
          e3 = boolWrite p6 True e2
          e4 = boolWrite p8 False e3
          e5 = boolWrite p9 True e4
          -- Extra bits: value - 19 as 4-bit MSB-first
          extra = absVal - 19
          probs = pcatProbs V.! 3
          e6 = boolWrite (probs VU.! 0) (testBit extra 3) e5
          e7 = boolWrite (probs VU.! 1) (testBit extra 2) e6
          e8 = boolWrite (probs VU.! 2) (testBit extra 1) e7
          e9 = boolWrite (probs VU.! 3) (testBit extra 0) e8
       in boolWrite 128 (coeff < 0) e9
  | absVal <= 66 =
      -- CAT5 (35-66): p[2]=T, p[3]=T, p[6]=T, p[8]=T, p[10]=F (actually p[9+bit1])
      -- libwebp: bit1=VP8GetBit(p[8]), bit0=VP8GetBit(p[9+bit1]), cat=2*bit1+bit0
      -- CAT5: bit1=1, bit0=0 → cat=2, but that gives CAT5 (3+(8<<2)=35)
      -- Actually: p[8]=True, p[9+1]=p[10]=False → cat=2*1+0=2 → CAT5
      let e1 = boolWrite p2 True enc
          e2 = boolWrite p3 True e1
          e3 = boolWrite p6 True e2
          e4 = boolWrite p8 True e3
          e5 = boolWrite p10 False e4
          -- Extra bits: value - 35 as 5-bit MSB-first
          extra = absVal - 35
          probs = pcatProbs V.! 4
          e6 = boolWrite (probs VU.! 0) (testBit extra 4) e5
          e7 = boolWrite (probs VU.! 1) (testBit extra 3) e6
          e8 = boolWrite (probs VU.! 2) (testBit extra 2) e7
          e9 = boolWrite (probs VU.! 3) (testBit extra 1) e8
          e10 = boolWrite (probs VU.! 4) (testBit extra 0) e9
       in boolWrite 128 (coeff < 0) e10
  | otherwise =
      -- CAT6 (67-2048): p[2]=T, p[3]=T, p[6]=T, p[8]=T, p[10]=T (p[9+1]=True)
      -- Actually: p[8]=True, p[10]=True → cat=2*1+1=3 → CAT6 (3+(8<<3)=67)
      let e1 = boolWrite p2 True enc
          e2 = boolWrite p3 True e1
          e3 = boolWrite p6 True e2
          e4 = boolWrite p8 True e3
          e5 = boolWrite p10 True e4
          -- Extra bits: value - 67 as 11-bit MSB-first
          extra = absVal - 67
          probs = pcatProbs V.! 5
          e6 = boolWrite (probs VU.! 0) (testBit extra 10) e5
          e7 = boolWrite (probs VU.! 1) (testBit extra 9) e6
          e8 = boolWrite (probs VU.! 2) (testBit extra 8) e7
          e9 = boolWrite (probs VU.! 3) (testBit extra 7) e8
          e10 = boolWrite (probs VU.! 4) (testBit extra 6) e9
          e11 = boolWrite (probs VU.! 5) (testBit extra 5) e10
          e12 = boolWrite (probs VU.! 6) (testBit extra 4) e11
          e13 = boolWrite (probs VU.! 7) (testBit extra 3) e12
          e14 = boolWrite (probs VU.! 8) (testBit extra 2) e13
          e15 = boolWrite (probs VU.! 9) (testBit extra 1) e14
          e16 = boolWrite (probs VU.! 10) (testBit extra 0) e15
       in boolWrite 128 (coeff < 0) e16

-- | Count coefficient branch statistics for probability optimization.
-- Mirrors encodeCoefficients exactly but records branch decisions
-- to a statistics accumulator instead of writing to a BoolEncoder.
countCoefficients ::
  VSM.MVector s Int16 -> -- Quantized coefficients (in raster scan order from FDCT)
  Int -> -- Block type (libwebp: 0=i16-AC/Y-AC, 1=i16-DC/Y2, 2=chroma, 3=i4-AC)
  Int -> -- Initial context (0, 1, or 2)
  Int -> -- Start position (0 or 1)
  CoeffStats s -> -- Statistics accumulator
  ST s ()
countCoefficients coeffs blockType initialCtx startPos stats = do
  lastNzPos <- findLastNonzero coeffs startPos
  case lastNzPos of
    Nothing -> do
      let !band = coeffBands VU.! startPos
          !probIdx = blockType * 264 + band * 33 + initialCtx * 11
      recordBranch stats probIdx False -- EOB
    Just lastNz -> do
      let loop !pos !ctx !skipEOB
            | pos > lastNz =
                if pos < 16 && not skipEOB
                  then do
                    let !band = coeffBands VU.! pos
                        !probIdx = blockType * 264 + band * 33 + ctx * 11
                    recordBranch stats probIdx False -- EOB
                  else return ()
            | otherwise = do
                let !zigzagIdx = zigzag VU.! pos
                coeff <- VSM.read coeffs zigzagIdx
                let !band = coeffBands VU.! pos
                    !probIdx = blockType * 264 + band * 33 + ctx * 11
                if coeff == 0
                  then do
                    if skipEOB
                      then recordBranch stats (probIdx + 1) False -- p[1]=False
                      else do
                        recordBranch stats probIdx True -- p[0]=True (not EOB)
                        recordBranch stats (probIdx + 1) False -- p[1]=False (zero)
                    loop (pos + 1) 0 True
                  else do
                    let !absCoeff = abs (fromIntegral coeff :: Int)
                    if skipEOB
                      then return ()
                      else recordBranch stats probIdx True -- p[0]=True (not EOB)
                    recordBranch stats (probIdx + 1) True -- p[1]=True (nonzero)
                    countValue stats probIdx absCoeff
                    let !newCtx = if absCoeff == 1 then 1 else 2
                    loop (pos + 1) newCtx False
      loop startPos initialCtx False
  where
    findLastNonzero cs start = go Nothing start
      where
        go lastFound pos
          | pos >= 16 = return lastFound
          | otherwise = do
              let !zigzagIdx = zigzag VU.! pos
              coeff <- VSM.read cs zigzagIdx
              let !newLast = if coeff /= 0 then Just pos else lastFound
              go newLast (pos + 1)

-- | Count value-encoding branches, mirroring encodeValue exactly.
{-# INLINE countValue #-}
countValue :: CoeffStats s -> Int -> Int -> ST s ()
countValue !stats !probIdx !absVal
  | absVal == 1 =
      recordBranch stats (probIdx + 2) False -- p[2]=False (value is 1)
  | absVal == 2 = do
      recordBranch stats (probIdx + 2) True
      recordBranch stats (probIdx + 3) False
      recordBranch stats (probIdx + 4) False
  | absVal == 3 = do
      recordBranch stats (probIdx + 2) True
      recordBranch stats (probIdx + 3) False
      recordBranch stats (probIdx + 4) True
      recordBranch stats (probIdx + 5) False
  | absVal == 4 = do
      recordBranch stats (probIdx + 2) True
      recordBranch stats (probIdx + 3) False
      recordBranch stats (probIdx + 4) True
      recordBranch stats (probIdx + 5) True
  | absVal <= 6 = do
      -- CAT1 (5-6)
      recordBranch stats (probIdx + 2) True
      recordBranch stats (probIdx + 3) True
      recordBranch stats (probIdx + 6) False
      recordBranch stats (probIdx + 7) False
  | absVal <= 10 = do
      -- CAT2 (7-10)
      recordBranch stats (probIdx + 2) True
      recordBranch stats (probIdx + 3) True
      recordBranch stats (probIdx + 6) False
      recordBranch stats (probIdx + 7) True
  | absVal <= 18 = do
      -- CAT3 (11-18)
      recordBranch stats (probIdx + 2) True
      recordBranch stats (probIdx + 3) True
      recordBranch stats (probIdx + 6) True
      recordBranch stats (probIdx + 8) False
      recordBranch stats (probIdx + 9) False
  | absVal <= 34 = do
      -- CAT4 (19-34)
      recordBranch stats (probIdx + 2) True
      recordBranch stats (probIdx + 3) True
      recordBranch stats (probIdx + 6) True
      recordBranch stats (probIdx + 8) False
      recordBranch stats (probIdx + 9) True
  | absVal <= 66 = do
      -- CAT5 (35-66)
      recordBranch stats (probIdx + 2) True
      recordBranch stats (probIdx + 3) True
      recordBranch stats (probIdx + 6) True
      recordBranch stats (probIdx + 8) True
      recordBranch stats (probIdx + 10) False
  | otherwise = do
      -- CAT6 (67-2048)
      recordBranch stats (probIdx + 2) True
      recordBranch stats (probIdx + 3) True
      recordBranch stats (probIdx + 6) True
      recordBranch stats (probIdx + 8) True
      recordBranch stats (probIdx + 10) True
