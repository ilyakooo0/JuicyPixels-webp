{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.Coefficients
  ( decodeCoefficients,
  )
where

import Codec.Picture.WebP.Internal.VP8.BoolDecoder
import Codec.Picture.WebP.Internal.VP8.Tables
import Control.Monad (when)
import Control.Monad.ST
import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Decode DCT coefficients for a 4x4 block
-- Returns: (coefficients, has_nonzero, updated decoder)
decodeCoefficients ::
  BoolDecoder ->
  VU.Vector Word8 ->
  Int ->
  Int ->
  Int ->
  ST s (VSM.MVector s Int16, Bool, BoolDecoder)
decodeCoefficients decoder coeffProbs blockType initialCtx startPos = do
  coeffs <- VSM.replicate 16 0

  let loop !pos !ctx !d !hasNonzero !skipEOB
        | pos >= 16 = do
            return (coeffs, hasNonzero, d)
        | otherwise = do
            let band = coeffBands VU.! pos
                probIdx = blockType * 264 + band * 33 + ctx * 11

            let treeStart = if skipEOB then 2 else 0
                (token, d1) = boolReadTree (V.drop treeStart coeffTree) (getCoeffProbs coeffProbs probIdx treeStart) d

            case token of
              0 -> loop (pos + 1) 0 d1 hasNonzero True
              11 ->
                return (coeffs, hasNonzero, d1)
              _ -> do
                let (value, d2) = decodeCoeffValue token d1
                    zigzagPos = zigzag VU.! pos
                VSM.write coeffs zigzagPos value
                let newCtx = if abs value == 1 then 1 else 2
                loop (pos + 1) newCtx d2 True False

  loop startPos initialCtx decoder False False

-- | Decode coefficient value from token
decodeCoeffValue :: Int -> BoolDecoder -> (Int16, BoolDecoder)
decodeCoeffValue token decoder
  | token >= 1 && token <= 4 =
      let (sign, d) = boolRead 128 decoder
          value = if sign then -fromIntegral token else fromIntegral token
       in (value, d)
  | token == 5 = decodeCat1 decoder
  | token == 6 = decodeCat2 decoder
  | token == 7 = decodeCat3 decoder
  | token == 8 = decodeCat4 decoder
  | token == 9 = decodeCat5 decoder
  | token == 10 = decodeCat6 decoder
  | otherwise = (0, decoder)

-- | Decode CAT1 (5-6)
decodeCat1 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat1 decoder =
  let probs = pcatProbs V.! 0
      (bit0, d1) = boolRead (probs VU.! 0) decoder
      value = 5 + if bit0 then 1 else 0
      (sign, d2) = boolRead 128 d1
   in (if sign then -value else value, d2)

-- | Decode CAT2 (7-10)
decodeCat2 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat2 decoder =
  let probs = pcatProbs V.! 1
      (bit0, d1) = boolRead (probs VU.! 0) decoder
      (bit1, d2) = boolRead (probs VU.! 1) d1
      value = 7 + (if bit0 then 2 else 0) + (if bit1 then 1 else 0)
      (sign, d3) = boolRead 128 d2
   in (if sign then -value else value, d3)

-- | Decode CAT3 (11-18)
decodeCat3 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat3 decoder =
  let probs = pcatProbs V.! 2
      (bit0, d1) = boolRead (probs VU.! 0) decoder
      (bit1, d2) = boolRead (probs VU.! 1) d1
      (bit2, d3) = boolRead (probs VU.! 2) d2
      value = 11 + (if bit0 then 4 else 0) + (if bit1 then 2 else 0) + (if bit2 then 1 else 0)
      (sign, d4) = boolRead 128 d3
   in (if sign then -value else value, d4)

-- | Decode CAT4 (19-34)
decodeCat4 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat4 decoder =
  let probs = pcatProbs V.! 3
      (bit0, d1) = boolRead (probs VU.! 0) decoder
      (bit1, d2) = boolRead (probs VU.! 1) d1
      (bit2, d3) = boolRead (probs VU.! 2) d2
      (bit3, d4) = boolRead (probs VU.! 3) d3
      value =
        19
          + (if bit0 then 8 else 0)
          + (if bit1 then 4 else 0)
          + (if bit2 then 2 else 0)
          + (if bit3 then 1 else 0)
      (sign, d5) = boolRead 128 d4
   in (if sign then -value else value, d5)

-- | Decode CAT5 (35-66)
decodeCat5 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat5 decoder =
  let probs = pcatProbs V.! 4
      (bit0, d1) = boolRead (probs VU.! 0) decoder
      (bit1, d2) = boolRead (probs VU.! 1) d1
      (bit2, d3) = boolRead (probs VU.! 2) d2
      (bit3, d4) = boolRead (probs VU.! 3) d3
      (bit4, d5) = boolRead (probs VU.! 4) d4
      value =
        35
          + (if bit0 then 16 else 0)
          + (if bit1 then 8 else 0)
          + (if bit2 then 4 else 0)
          + (if bit3 then 2 else 0)
          + (if bit4 then 1 else 0)
      (sign, d6) = boolRead 128 d5
   in (if sign then -value else value, d6)

-- | Decode CAT6 (67-2048)
decodeCat6 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat6 decoder =
  let probs = pcatProbs V.! 5
      readBit i d = boolRead (probs VU.! i) d

      (bit0, d1) = readBit 0 decoder
      (bit1, d2) = readBit 1 d1
      (bit2, d3) = readBit 2 d2
      (bit3, d4) = readBit 3 d3
      (bit4, d5) = readBit 4 d4
      (bit5, d6) = readBit 5 d5
      (bit6, d7) = readBit 6 d6
      (bit7, d8) = readBit 7 d7
      (bit8, d9) = readBit 8 d8
      (bit9, d10) = readBit 9 d9
      (bit10, d11) = readBit 10 d10

      value =
        67
          + (if bit0 then 1024 else 0)
          + (if bit1 then 512 else 0)
          + (if bit2 then 256 else 0)
          + (if bit3 then 128 else 0)
          + (if bit4 then 64 else 0)
          + (if bit5 then 32 else 0)
          + (if bit6 then 16 else 0)
          + (if bit7 then 8 else 0)
          + (if bit8 then 4 else 0)
          + (if bit9 then 2 else 0)
          + (if bit10 then 1 else 0)

      (sign, d12) = boolRead 128 d11
   in (if sign then -value else value, d12)

-- | Get coefficient probabilities for a position
getCoeffProbs :: VU.Vector Word8 -> Int -> Int -> V.Vector Word8
getCoeffProbs probs baseIdx offset =
  V.generate (11 - offset) $ \i -> probs VU.! (baseIdx + offset + i)
