{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.EncodeCoefficients
  ( encodeCoefficients,
  )
where

import Codec.Picture.WebP.Internal.VP8.BoolEncoder
import Codec.Picture.WebP.Internal.VP8.Tables
import Control.Monad (when)
import Control.Monad.ST
import Data.Bits
import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Encode DCT coefficients for a 4x4 block
-- Returns: (updated encoder, has_nonzero)
encodeCoefficients ::
  VSM.MVector s Int16 -> -- Quantized coefficients (in raster scan order from FDCT)
  VU.Vector Word8 -> -- Coefficient probabilities
  Int -> -- Block type (0=Y AC, 1=Y2, 2=UV, 3=Y full)
  Int -> -- Initial context (0, 1, or 2)
  Int -> -- Start position (0 or 1)
  BoolEncoder ->
  ST s (BoolEncoder, Bool)
encodeCoefficients coeffs coeffProbs blockType initialCtx startPos encoder = do
  -- Convert coeffTree from boxed to unboxed vector
  let coeffTreeU = V.convert coeffTree :: VU.Vector Int8

  -- First, find the last nonzero coefficient position
  lastNzPos <- findLastNonzero coeffs startPos

  case lastNzPos of
    Nothing -> do
      -- All zeros: write EOB immediately at the start position
      let band = coeffBands VU.! startPos
          probIdx = blockType * 264 + band * 33 + initialCtx * 11
          enc' = boolWriteTree coeffTreeU (getCoeffProbs coeffProbs probIdx 0) 11 encoder
      return (enc', False)
    Just lastNz -> do
      -- There are nonzeros: encode coefficients up to and including lastNz, then EOB
      -- Track skipEOB: after DCT_0, skip the first bit in tree encoding
      let loop !pos !ctx !enc !skipEOB
            | pos > lastNz = do
                -- After the last nonzero, write EOB (token 11)
                -- But if we've encoded all 16 positions, no EOB needed (decoder knows to stop)
                if pos >= 16
                  then return (enc, True) -- No EOB needed at position 16
                  else do
                    let band = coeffBands VU.! pos
                        probIdx = blockType * 264 + band * 33 + ctx * 11
                        enc' = encodeTokenWithSkip coeffTreeU (getCoeffProbs coeffProbs probIdx 0) 11 skipEOB enc
                    return (enc', True)
            | otherwise = do
                -- Read coefficient in zigzag scan order
                let zigzagIdx = zigzag VU.! pos
                coeff <- VSM.read coeffs zigzagIdx
                let band = coeffBands VU.! pos
                    probIdx = blockType * 264 + band * 33 + ctx * 11

                if coeff == 0
                  then do
                    -- Token 0 (DCT_0)
                    let enc' = encodeTokenWithSkip coeffTreeU (getCoeffProbs coeffProbs probIdx 0) 0 skipEOB enc
                    -- After DCT_0, next position should skip EOB
                    loop (pos + 1) 0 enc' True
                  else do
                    -- Nonzero coefficient
                    let (token, extraBits) = coeffToToken coeff
                        enc1 = encodeTokenWithSkip coeffTreeU (getCoeffProbs coeffProbs probIdx 0) token skipEOB enc
                        enc2 = encodeExtraBits token extraBits coeff enc1
                        newCtx = if abs coeff == 1 then 1 else 2
                    -- After nonzero, don't skip EOB
                    loop (pos + 1) newCtx enc2 False

      loop startPos initialCtx encoder False
  where
    -- Find the last nonzero coefficient position (in zigzag order)
    findLastNonzero cs start = go Nothing start
      where
        go lastFound pos
          | pos >= 16 = return lastFound
          | otherwise = do
              let zigzagIdx = zigzag VU.! pos
              coeff <- VSM.read cs zigzagIdx
              let newLast = if coeff /= 0 then Just pos else lastFound
              go newLast (pos + 1)

-- | Encode a token, optionally skipping the first bit (for skipEOB after DCT_0)
-- When skipEOB is True, we find the path through the full tree but skip writing
-- the first bit, since the decoder knows we're not at EOB
{-# INLINE encodeTokenWithSkip #-}
encodeTokenWithSkip ::
  VU.Vector Int8 ->
  VU.Vector Word8 ->
  Int ->
  Bool -> -- skipEOB
  BoolEncoder ->
  BoolEncoder
encodeTokenWithSkip tree probs targetValue skipEOB enc =
  case findPath 0 0 [] of
    Just path ->
      let fullPath = reverse path
          -- When skipEOB is True, the first bit would always be True (go right from EOB)
          -- so we skip it and the first probability
          actualPath = if skipEOB then drop 1 fullPath else fullPath
          actualProbs = if skipEOB then VU.drop 1 probs else probs
       in writePath actualPath actualProbs 0 enc
    Nothing -> error $ "VP8 encoder: value " ++ show targetValue ++ " not in tree"
  where
    findPath !i !depth !path
      | i >= VU.length tree || i + 1 >= VU.length tree = Nothing
      | otherwise =
          let leftNode = tree VU.! i
              rightNode = tree VU.! (i + 1)
           in case checkNode leftNode (False : path) of
                Just p -> Just p
                Nothing -> checkNode rightNode (True : path)
      where
        checkNode node path'
          | node == 0 = if targetValue == 0 then Just path' else Nothing
          | node < 0 = if fromIntegral (negate node) == targetValue then Just path' else Nothing
          | otherwise = findPath (fromIntegral node) (depth + 1) path'

    writePath [] _ _ e = e
    writePath (bit : rest) ps probIdx e =
      writePath rest ps (probIdx + 1) (boolWrite (ps VU.! probIdx) bit e)

-- | Map coefficient value to token and extra bits
coeffToToken :: Int16 -> (Int, Int)
coeffToToken coeff
  | absVal >= 1 && absVal <= 4 = (fromIntegral absVal, 0)
  | absVal >= 5 && absVal <= 6 = (5, absVal - 5) -- CAT1
  | absVal >= 7 && absVal <= 10 = (6, absVal - 7) -- CAT2
  | absVal >= 11 && absVal <= 18 = (7, absVal - 11) -- CAT3
  | absVal >= 19 && absVal <= 34 = (8, absVal - 19) -- CAT4
  | absVal >= 35 && absVal <= 66 = (9, absVal - 35) -- CAT5
  | absVal >= 67 && absVal <= 2048 = (10, absVal - 67) -- CAT6
  | otherwise = (0, 0) -- Shouldn't happen for valid coefficients
  where
    absVal = abs (fromIntegral coeff :: Int)

-- | Encode extra bits for category tokens
{-# INLINE encodeExtraBits #-}
encodeExtraBits :: Int -> Int -> Int16 -> BoolEncoder -> BoolEncoder
encodeExtraBits token extraBits coeff enc
  | token >= 1 && token <= 4 =
      -- Literals: just write sign bit
      boolWrite 128 (coeff < 0) enc
  | token == 5 = encodeCat1 extraBits coeff enc
  | token == 6 = encodeCat2 extraBits coeff enc
  | token == 7 = encodeCat3 extraBits coeff enc
  | token == 8 = encodeCat4 extraBits coeff enc
  | token == 9 = encodeCat5 extraBits coeff enc
  | token == 10 = encodeCat6 extraBits coeff enc
  | otherwise = enc

-- | Encode CAT1 (5-6): 1 extra bit + sign
encodeCat1 :: Int -> Int16 -> BoolEncoder -> BoolEncoder
encodeCat1 extraBits coeff enc =
  let probs = pcatProbs V.! 0
      bit0 = testBit extraBits 0
      enc1 = boolWrite (probs VU.! 0) bit0 enc
      enc2 = boolWrite 128 (coeff < 0) enc1
   in enc2

-- | Encode CAT2 (7-10): 2 extra bits + sign
encodeCat2 :: Int -> Int16 -> BoolEncoder -> BoolEncoder
encodeCat2 extraBits coeff enc =
  let probs = pcatProbs V.! 1
      bit0 = testBit extraBits 1 -- MSB first
      bit1 = testBit extraBits 0
      enc1 = boolWrite (probs VU.! 0) bit0 enc
      enc2 = boolWrite (probs VU.! 1) bit1 enc1
      enc3 = boolWrite 128 (coeff < 0) enc2
   in enc3

-- | Encode CAT3 (11-18): 3 extra bits + sign
encodeCat3 :: Int -> Int16 -> BoolEncoder -> BoolEncoder
encodeCat3 extraBits coeff enc =
  let probs = pcatProbs V.! 2
      bit0 = testBit extraBits 2 -- MSB first
      bit1 = testBit extraBits 1
      bit2 = testBit extraBits 0
      enc1 = boolWrite (probs VU.! 0) bit0 enc
      enc2 = boolWrite (probs VU.! 1) bit1 enc1
      enc3 = boolWrite (probs VU.! 2) bit2 enc2
      enc4 = boolWrite 128 (coeff < 0) enc3
   in enc4

-- | Encode CAT4 (19-34): 4 extra bits + sign
encodeCat4 :: Int -> Int16 -> BoolEncoder -> BoolEncoder
encodeCat4 extraBits coeff enc =
  let probs = pcatProbs V.! 3
      bit0 = testBit extraBits 3 -- MSB first
      bit1 = testBit extraBits 2
      bit2 = testBit extraBits 1
      bit3 = testBit extraBits 0
      enc1 = boolWrite (probs VU.! 0) bit0 enc
      enc2 = boolWrite (probs VU.! 1) bit1 enc1
      enc3 = boolWrite (probs VU.! 2) bit2 enc2
      enc4 = boolWrite (probs VU.! 3) bit3 enc3
      enc5 = boolWrite 128 (coeff < 0) enc4
   in enc5

-- | Encode CAT5 (35-66): 5 extra bits + sign
encodeCat5 :: Int -> Int16 -> BoolEncoder -> BoolEncoder
encodeCat5 extraBits coeff enc =
  let probs = pcatProbs V.! 4
      bit0 = testBit extraBits 4 -- MSB first
      bit1 = testBit extraBits 3
      bit2 = testBit extraBits 2
      bit3 = testBit extraBits 1
      bit4 = testBit extraBits 0
      enc1 = boolWrite (probs VU.! 0) bit0 enc
      enc2 = boolWrite (probs VU.! 1) bit1 enc1
      enc3 = boolWrite (probs VU.! 2) bit2 enc2
      enc4 = boolWrite (probs VU.! 3) bit3 enc3
      enc5 = boolWrite (probs VU.! 4) bit4 enc4
      enc6 = boolWrite 128 (coeff < 0) enc5
   in enc6

-- | Encode CAT6 (67-2048): 11 extra bits + sign
encodeCat6 :: Int -> Int16 -> BoolEncoder -> BoolEncoder
encodeCat6 extraBits coeff enc =
  let probs = pcatProbs V.! 5
      writeBit i e = boolWrite (probs VU.! i) (testBit extraBits (10 - i)) e -- MSB first
      enc1 = writeBit 0 enc
      enc2 = writeBit 1 enc1
      enc3 = writeBit 2 enc2
      enc4 = writeBit 3 enc3
      enc5 = writeBit 4 enc4
      enc6 = writeBit 5 enc5
      enc7 = writeBit 6 enc6
      enc8 = writeBit 7 enc7
      enc9 = writeBit 8 enc8
      enc10 = writeBit 9 enc9
      enc11 = writeBit 10 enc10
      enc12 = boolWrite 128 (coeff < 0) enc11
   in enc12

-- | Get coefficient probabilities starting at index
{-# INLINE getCoeffProbs #-}
getCoeffProbs :: VU.Vector Word8 -> Int -> Int -> VU.Vector Word8
getCoeffProbs allProbs startIdx offset =
  VU.drop (startIdx + offset) allProbs
