{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.BoolEncoder
  ( BoolEncoder,
    initBoolEncoder,
    boolWrite,
    boolWriteLiteral,
    boolWriteSigned,
    boolWriteTree,
    finalizeBoolEncoder,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import Data.Int
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Boolean arithmetic encoder
-- Exact inverse of BoolDecoder algorithm
data BoolEncoder = BoolEncoder
  { beBytes :: ![Word8],
    beRange :: !Word32,
    beValue :: !Word32,
    beCount :: !Int,
    beBitCount :: !Int -- Total bits written
  }

initBoolEncoder :: BoolEncoder
initBoolEncoder =
  BoolEncoder
    { beBytes = [],
      beRange = 255,
      beValue = 0, -- Start at bottom of range
      beCount = 0,
      beBitCount = 0
    }

-- | Write one bit - encoder maintains bottom of current interval
boolWrite :: Word8 -> Bool -> BoolEncoder -> BoolEncoder
boolWrite !prob !bit !enc =
  let !split = 1 + (((beRange enc - 1) * fromIntegral prob) `shiftR` 8)
      (!newRange, !newValue) =
        if bit
          then (beRange enc - split, beValue enc + (split `shiftL` 8))
          else (split, beValue enc)
   in renorm enc {beRange = newRange, beValue = newValue, beBitCount = beBitCount enc + 1}

-- | Renormalize - shift and output bytes
renorm :: BoolEncoder -> BoolEncoder
renorm !enc
  | beRange enc >= 128 = enc
  | beCount enc >= 8 =
      -- Output a byte
      let !byte = fromIntegral ((beValue enc `shiftR` 8) .&. 0xFF) :: Word8
          !enc' =
            enc
              { beBytes = byte : beBytes enc,
                beRange = beRange enc `shiftL` 1,
                beValue = (beValue enc `shiftL` 1) .&. 0xFFFF,
                beCount = beCount enc - 7
              }
       in renorm enc'
  | otherwise =
      renorm
        enc
          { beRange = beRange enc `shiftL` 1,
            beValue = (beValue enc `shiftL` 1) .&. 0xFFFF,
            beCount = beCount enc + 1
          }

-- | Finalize and output
finalizeBoolEncoder :: BoolEncoder -> B.ByteString
finalizeBoolEncoder !enc =
  let -- Flush remaining
      finalEnc =
        if beCount enc > 0
          then
            let byte = fromIntegral ((beValue enc `shiftR` 8) .&. 0xFF) :: Word8
             in enc {beBytes = byte : beBytes enc}
          else enc

      allBytes = reverse (beBytes finalEnc)

      -- Initial 2 bytes
      header0 = fromIntegral ((beValue enc `shiftR` 8) .&. 0xFF) :: Word8
      header1 = fromIntegral (beValue enc .&. 0xFF) :: Word8

   in B.pack (header0 : header1 : allBytes)

boolWriteLiteral :: Int -> Word32 -> BoolEncoder -> BoolEncoder
boolWriteLiteral 0 _ !enc = enc
boolWriteLiteral !n !value !enc =
  let !bitPos = n - 1
      !bit = testBit value bitPos
   in boolWriteLiteral (n - 1) value (boolWrite 128 bit enc)

boolWriteSigned :: Int -> Int32 -> BoolEncoder -> BoolEncoder
boolWriteSigned !n !value !enc =
  let !absValue = abs value
      !enc' = boolWriteLiteral n (fromIntegral absValue) enc
   in boolWrite 128 (value < 0) enc'

boolWriteTree :: VU.Vector Int8 -> VU.Vector Word8 -> Int -> BoolEncoder -> BoolEncoder
boolWriteTree !tree !probs !targetValue !enc =
  case findPath tree targetValue 0 1 [] of
    Just path -> writePath (reverse path) probs 0 enc
    Nothing -> error $ "VP8 encoder: value " ++ show targetValue ++ " not in tree"
  where
    findPath t val leftIdx rightIdx path
      | leftIdx >= VU.length t || rightIdx >= VU.length t = Nothing
      | otherwise =
          let leftNode = t VU.! leftIdx
              rightNode = t VU.! rightIdx
           in case checkNode t val leftNode (False : path) of
                Just p -> Just p
                Nothing -> checkNode t val rightNode (True : path)

    checkNode t val node path
      | node == 0 = if val == 0 then Just path else Nothing
      | node < 0 = if fromIntegral (negate node) == val then Just path else Nothing
      | otherwise =
          let baseIdx = fromIntegral node
           in if baseIdx + 1 < VU.length t
                then findPath t val baseIdx (baseIdx + 1) path
                else Nothing

    writePath [] _ _ e = e
    writePath (bit : rest) ps probIdx e =
      writePath rest ps (probIdx + 1) (boolWrite (ps VU.! probIdx) bit e)
