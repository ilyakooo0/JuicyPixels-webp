{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.Coefficients
  ( decodeCoefficients,
  )
where

import Codec.Picture.WebP.Internal.VP8.BoolDecoder
import Codec.Picture.WebP.Internal.VP8.Tables
import Control.Monad.ST
import Data.Int
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Decode DCT coefficients for a 4x4 block using flat probability indexing.
-- This matches libwebp's GetCoeffsFast exactly: p[0]=EOB, p[1]=zero,
-- p[2]=is_one, p[3..5]=small values (2-4), p[6..10]=large values (CAT1-6).
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
            let !band = coeffBands `VU.unsafeIndex` pos
                !probIdx = blockType * 264 + band * 33 + ctx * 11

            if skipEOB
              then do
                -- After DCT_0: skip p[0] (EOB check), read p[1] directly
                let (!isNonzero, !d1) = boolRead (coeffProbs `VU.unsafeIndex` (probIdx + 1)) d
                if not isNonzero
                  then -- DCT_0 (zero): advance to next position
                    loop (pos + 1) 0 d1 hasNonzero True
                  else do
                    -- Nonzero: decode value using p[2..10]
                    let (!value, !d2) = decodeValueFlat coeffProbs probIdx d1
                        !zigzagPos = zigzag `VU.unsafeIndex` pos
                    VSM.unsafeWrite coeffs zigzagPos value
                    let !newCtx = if abs value == 1 then 1 else 2
                    loop (pos + 1) newCtx d2 True False
              else do
                -- Normal: check p[0] for EOB
                let (!notEOB, !d1) = boolRead (coeffProbs `VU.unsafeIndex` probIdx) d
                if not notEOB
                  then -- EOB: done with this block
                    return (coeffs, hasNonzero, d1)
                  else do
                    -- Not EOB: check p[1] for zero/nonzero
                    let (!isNonzero, !d2) = boolRead (coeffProbs `VU.unsafeIndex` (probIdx + 1)) d1
                    if not isNonzero
                      then -- DCT_0 (zero): advance with skipEOB
                        loop (pos + 1) 0 d2 hasNonzero True
                      else do
                        -- Nonzero: decode value using p[2..10]
                        let (!value, !d3) = decodeValueFlat coeffProbs probIdx d2
                            !zigzagPos = zigzag `VU.unsafeIndex` pos
                        VSM.unsafeWrite coeffs zigzagPos value
                        let !newCtx = if abs value == 1 then 1 else 2
                        loop (pos + 1) newCtx d3 True False

  loop startPos initialCtx decoder False False

-- | Decode a nonzero coefficient value using flat p[2..10] indices.
-- Matches libwebp's GetCoeffsFast + GetLargeValue.
-- Called after p[1]=True (nonzero confirmed).
{-# INLINE decodeValueFlat #-}
decodeValueFlat :: VU.Vector Word8 -> Int -> BoolDecoder -> (Int16, BoolDecoder)
decodeValueFlat coeffProbs probIdx decoder =
  let (!isLarge, !d1) = boolRead (coeffProbs `VU.unsafeIndex` (probIdx + 2)) decoder -- p[2]
   in if not isLarge
        then
          -- Value is 1
          let (!sign, !d2) = boolRead 128 d1
           in (if sign then -1 else 1, d2)
        else decodeLargeValue coeffProbs probIdx d1

-- | Decode values >= 2 using p[3..10]. Matches libwebp's GetLargeValue.
{-# INLINE decodeLargeValue #-}
decodeLargeValue :: VU.Vector Word8 -> Int -> BoolDecoder -> (Int16, BoolDecoder)
decodeLargeValue coeffProbs probIdx decoder =
  let (!isCategory, !d1) = boolRead (coeffProbs `VU.unsafeIndex` (probIdx + 3)) decoder -- p[3]
   in if not isCategory
        then
          -- Small values (2-4): p[4], p[5]
          let (!not2, !d2) = boolRead (coeffProbs `VU.unsafeIndex` (probIdx + 4)) d1 -- p[4]
           in if not not2
                then
                  -- Value 2
                  let (!sign, !d3) = boolRead 128 d2
                   in (if sign then -2 else 2, d3)
                else
                  -- Value 3 or 4: p[5]
                  let (!is4, !d3) = boolRead (coeffProbs `VU.unsafeIndex` (probIdx + 5)) d2 -- p[5]
                      !v = if is4 then 4 else 3
                      (!sign, !d4) = boolRead 128 d3
                   in (if sign then -v else v, d4)
        else
          -- Category values (5+): p[6], p[7] or p[8], p[9], p[10]
          let (!isBigCat, !d2) = boolRead (coeffProbs `VU.unsafeIndex` (probIdx + 6)) d1 -- p[6]
           in if not isBigCat
                then
                  -- CAT1 or CAT2: p[7]
                  let (!isCat2, !d3) = boolRead (coeffProbs `VU.unsafeIndex` (probIdx + 7)) d2 -- p[7]
                   in if not isCat2
                        then decodeCat1 d3 -- CAT1 (5-6)
                        else decodeCat2 d3 -- CAT2 (7-10)
                else
                  -- CAT3-6: p[8], p[9+bit1]
                  let (!bit1, !d3) = boolRead (coeffProbs `VU.unsafeIndex` (probIdx + 8)) d2 -- p[8]
                      !p9idx = probIdx + 9 + (if bit1 then 1 else 0) -- p[9] or p[10]
                      (!bit0, !d4) = boolRead (coeffProbs `VU.unsafeIndex` p9idx) d3
                      !cat = 2 * (if bit1 then 1 else 0) + (if bit0 then 1 else 0) :: Int
                   in case cat of
                        0 -> decodeCat3 d4 -- CAT3 (11-18)
                        1 -> decodeCat4 d4 -- CAT4 (19-34)
                        2 -> decodeCat5 d4 -- CAT5 (35-66)
                        _ -> decodeCat6 d4 -- CAT6 (67-2048)

-- | Decode CAT1 (5-6)
{-# INLINE decodeCat1 #-}
decodeCat1 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat1 decoder =
  let (!bit0, !d1) = boolRead (pcatProbs1 `VU.unsafeIndex` 0) decoder
      !value = 5 + if bit0 then 1 else 0
      (!sign, !d2) = boolRead 128 d1
   in (if sign then -value else value, d2)

-- | Decode CAT2 (7-10)
{-# INLINE decodeCat2 #-}
decodeCat2 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat2 decoder =
  let (!bit0, !d1) = boolRead (pcatProbs2 `VU.unsafeIndex` 0) decoder
      (!bit1, !d2) = boolRead (pcatProbs2 `VU.unsafeIndex` 1) d1
      !value = 7 + (if bit0 then 2 else 0) + (if bit1 then 1 else 0)
      (!sign, !d3) = boolRead 128 d2
   in (if sign then -value else value, d3)

-- | Decode CAT3 (11-18)
{-# INLINE decodeCat3 #-}
decodeCat3 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat3 decoder =
  let (!bit0, !d1) = boolRead (pcatProbs3 `VU.unsafeIndex` 0) decoder
      (!bit1, !d2) = boolRead (pcatProbs3 `VU.unsafeIndex` 1) d1
      (!bit2, !d3) = boolRead (pcatProbs3 `VU.unsafeIndex` 2) d2
      !value = 11 + (if bit0 then 4 else 0) + (if bit1 then 2 else 0) + (if bit2 then 1 else 0)
      (!sign, !d4) = boolRead 128 d3
   in (if sign then -value else value, d4)

-- | Decode CAT4 (19-34)
{-# INLINE decodeCat4 #-}
decodeCat4 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat4 decoder =
  let (!bit0, !d1) = boolRead (pcatProbs4 `VU.unsafeIndex` 0) decoder
      (!bit1, !d2) = boolRead (pcatProbs4 `VU.unsafeIndex` 1) d1
      (!bit2, !d3) = boolRead (pcatProbs4 `VU.unsafeIndex` 2) d2
      (!bit3, !d4) = boolRead (pcatProbs4 `VU.unsafeIndex` 3) d3
      !value =
        19
          + (if bit0 then 8 else 0)
          + (if bit1 then 4 else 0)
          + (if bit2 then 2 else 0)
          + (if bit3 then 1 else 0)
      (!sign, !d5) = boolRead 128 d4
   in (if sign then -value else value, d5)

-- | Decode CAT5 (35-66)
{-# INLINE decodeCat5 #-}
decodeCat5 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat5 decoder =
  let (!bit0, !d1) = boolRead (pcatProbs5 `VU.unsafeIndex` 0) decoder
      (!bit1, !d2) = boolRead (pcatProbs5 `VU.unsafeIndex` 1) d1
      (!bit2, !d3) = boolRead (pcatProbs5 `VU.unsafeIndex` 2) d2
      (!bit3, !d4) = boolRead (pcatProbs5 `VU.unsafeIndex` 3) d3
      (!bit4, !d5) = boolRead (pcatProbs5 `VU.unsafeIndex` 4) d4
      !value =
        35
          + (if bit0 then 16 else 0)
          + (if bit1 then 8 else 0)
          + (if bit2 then 4 else 0)
          + (if bit3 then 2 else 0)
          + (if bit4 then 1 else 0)
      (!sign, !d6) = boolRead 128 d5
   in (if sign then -value else value, d6)

-- | Decode CAT6 (67-2048)
decodeCat6 :: BoolDecoder -> (Int16, BoolDecoder)
decodeCat6 decoder =
  let !probs = pcatProbs6
      readBit i d = boolRead (probs `VU.unsafeIndex` i) d

      (!bit0, !d1) = readBit 0 decoder
      (!bit1, !d2) = readBit 1 d1
      (!bit2, !d3) = readBit 2 d2
      (!bit3, !d4) = readBit 3 d3
      (!bit4, !d5) = readBit 4 d4
      (!bit5, !d6) = readBit 5 d5
      (!bit6, !d7) = readBit 6 d6
      (!bit7, !d8) = readBit 7 d7
      (!bit8, !d9) = readBit 8 d8
      (!bit9, !d10) = readBit 9 d9
      (!bit10, !d11) = readBit 10 d10

      !value =
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

      (!sign, !d12) = boolRead 128 d11
   in (if sign then -value else value, d12)
