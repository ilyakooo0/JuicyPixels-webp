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

  let loop !pos !ctx !enc !hasNonzero !lastNonzeroPos
        | pos >= 16 = do
            -- Write EOB token if we haven't written all zeros
            if hasNonzero
              then do
                let band = coeffBands VU.! lastNonzeroPos
                    probIdx = blockType * 264 + band * 33 + ctx * 11
                    -- EOB is token 11
                    enc' = boolWriteTree coeffTreeU (getCoeffProbs coeffProbs probIdx 0) 11 enc
                return (enc', True)
              else return (enc, False)
        | otherwise = do
            -- Read coefficient in zigzag scan order
            -- Coefficients from FDCT are in raster order, need to map via zigzag
            let zigzagIdx = zigzag VU.! pos
            coeff <- VSM.read coeffs zigzagIdx

            -- DEBUG: For UV block type at pos 0, print the coefficient
            when (blockType == 2 && pos == 0 && coeff /= 0) $
              error $ "DEBUG ENCODE: UV coeff[0] = " ++ show coeff ++ " token will be " ++ show (fst $ coeffToToken coeff)

            if coeff == 0
              then do
                -- Token 0 (DCT_0)
                let band = coeffBands VU.! pos
                    probIdx = blockType * 264 + band * 33 + ctx * 11
                    skipEOB = pos > 0 && lastNonzeroPos == pos - 1 -- Skip EOB after DCT_0
                    treeStart = if skipEOB then 2 else 0
                    enc' = boolWriteTreeOffset coeffTreeU (getCoeffProbs coeffProbs probIdx treeStart) 0 treeStart enc
                loop (pos + 1) 0 enc' hasNonzero lastNonzeroPos
              else do
                -- Nonzero coefficient
                let band = coeffBands VU.! pos
                    probIdx = blockType * 264 + band * 33 + ctx * 11
                    (token, extraBits) = coeffToToken coeff
                    enc1 = boolWriteTree coeffTreeU (getCoeffProbs coeffProbs probIdx 0) token enc
                    enc2 = encodeExtraBits token extraBits coeff enc1
                    newCtx = if abs coeff == 1 then 1 else 2
                loop (pos + 1) newCtx enc2 True pos

  loop startPos initialCtx encoder False 0

-- | Write tree with offset (for skipping EOB branch after DCT_0)
boolWriteTreeOffset ::
  VU.Vector Int8 ->
  VU.Vector Word8 ->
  Int ->
  Int ->
  BoolEncoder ->
  BoolEncoder
boolWriteTreeOffset tree probs targetValue offset enc =
  -- For simplicity, rewrite the tree logic
  -- When offset=2, we skip the first branch (EOB)
  if offset == 0
    then boolWriteTree tree probs targetValue enc
    else boolWriteTree (VU.drop offset tree) (VU.drop (offset `div` 2) probs) targetValue enc

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
getCoeffProbs :: VU.Vector Word8 -> Int -> Int -> VU.Vector Word8
getCoeffProbs allProbs startIdx offset =
  VU.drop (startIdx + offset) allProbs
