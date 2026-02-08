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

-- | Boolean arithmetic encoder for VP8
-- Implements range coding matching the VP8 boolean decoder
data BoolEncoder = BoolEncoder
  { beBuffer :: ![Word8], -- Output bytes (reversed for efficiency)
    beRange :: !Word32, -- Current range [128..255]
    beValue :: !Word64, -- Current value (using 64-bit to avoid overflow)
    beCount :: !Int -- Bit count: starts at -24, increments on shift
  }
  deriving (Show)

-- | Initialize a fresh boolean encoder
initBoolEncoder :: BoolEncoder
initBoolEncoder =
  BoolEncoder
    { beBuffer = [],
      beRange = 255,
      beValue = 0,
      beCount = -24 -- Will output first byte after 24 shifts
    }

-- | Write a single bit with given probability
-- prob in [1..255] represents P(bit=0) = prob/256
boolWrite :: Word8 -> Bool -> BoolEncoder -> BoolEncoder
boolWrite !prob !bit !enc =
  let !split = 1 + (((beRange enc - 1) * fromIntegral prob) `shiftR` 8)
      (!newRange, !newValue) =
        if bit
          then
            -- Encode bit=1: use upper partition
            (beRange enc - split, beValue enc + fromIntegral split)
          else
            -- Encode bit=0: use lower partition
            (split, beValue enc)
   in renormalize enc {beRange = newRange, beValue = newValue}

-- | Renormalize: shift range and value, output bytes when needed
renormalize :: BoolEncoder -> BoolEncoder
renormalize !enc
  | beRange enc >= 128 = enc
  | otherwise =
      let !newRange = beRange enc `shiftL` 1
          !newValue = beValue enc `shiftL` 1
          !newCount = beCount enc + 1
       in if newCount >= 0
            then
              -- Time to output a byte
              -- Extract bits [31..24] of value (top byte)
              let !outByte = fromIntegral ((newValue `shiftR` 24) .&. 0xFF) :: Word8
                  -- Keep only lower 24 bits
                  !clearedValue = newValue .&. 0xFFFFFF
                  !enc' =
                    enc
                      { beBuffer = outByte : beBuffer enc,
                        beRange = newRange,
                        beValue = clearedValue,
                        beCount = newCount - 8
                      }
               in renormalize enc'
            else
              -- Not ready to output yet, continue shifting
              renormalize enc {beRange = newRange, beValue = newValue, beCount = newCount}

-- | Finalize encoder: flush remaining bits and create output
finalizeBoolEncoder :: BoolEncoder -> B.ByteString
finalizeBoolEncoder !enc =
  let -- First, shift to get count close to a byte boundary
      shiftToFlush !e
        | beCount e < 0 =
            -- Keep shifting until we're ready to output
            let !newValue = beValue e `shiftL` 1
                !newCount = beCount e + 1
             in shiftToFlush e {beValue = newValue, beCount = newCount}
        | otherwise = e

      !shifted = shiftToFlush enc

      -- Now flush all remaining bytes
      flushLoop !e !bytesWritten
        | bytesWritten >= 8 = e -- Prevent infinite loop, max 8 bytes
        | beValue e == 0 && bytesWritten > 0 = e -- Done if value is zero and we've written something
        | otherwise =
            let !byte = fromIntegral ((beValue e `shiftR` 24) .&. 0xFF) :: Word8
                !e' =
                  e
                    { beBuffer = byte : beBuffer e,
                      beValue = (beValue e `shiftL` 8) .&. 0xFFFFFFFFFFFFFFFF,
                      beCount = beCount e - 8
                    }
             in flushLoop e' (bytesWritten + 1)

      !flushed = flushLoop shifted 0
      !outputBytes = reverse (beBuffer flushed)

      -- The decoder reads first 2 bytes as initial value
      -- Write 0, 0 as standard (decoder compensates)
   in B.pack (0 : 0 : outputBytes)

-- | Write n-bit unsigned literal (MSB-first)
boolWriteLiteral :: Int -> Word32 -> BoolEncoder -> BoolEncoder
boolWriteLiteral 0 _ !enc = enc
boolWriteLiteral !n !value !enc =
  let !bitPos = n - 1
      !bit = testBit value bitPos
      !enc' = boolWrite 128 bit enc
   in boolWriteLiteral (n - 1) value enc'

-- | Write signed value (magnitude + sign)
boolWriteSigned :: Int -> Int32 -> BoolEncoder -> BoolEncoder
boolWriteSigned !n !value !enc =
  let !absValue = abs value
      !enc' = boolWriteLiteral n (fromIntegral absValue) enc
      !sign = value < 0
   in boolWrite 128 sign enc'

-- | Write a value using a tree
boolWriteTree :: VU.Vector Int8 -> VU.Vector Word8 -> Int -> BoolEncoder -> BoolEncoder
boolWriteTree !tree !probs !targetValue !enc =
  case findPathToPair tree targetValue 0 1 [] of
    Just path -> writePath (reverse path) probs 0 enc
    Nothing -> error $ "VP8 encoder: value " ++ show targetValue ++ " not in tree"
  where
    findPathToPair :: VU.Vector Int8 -> Int -> Int -> Int -> [Bool] -> Maybe [Bool]
    findPathToPair t val leftIdx rightIdx path
      | leftIdx >= VU.length t || rightIdx >= VU.length t = Nothing
      | otherwise =
          let leftNode = t VU.! leftIdx
              rightNode = t VU.! rightIdx
           in case checkNode t val leftNode (False : path) of
                Just p -> Just p
                Nothing -> checkNode t val rightNode (True : path)

    checkNode :: VU.Vector Int8 -> Int -> Int8 -> [Bool] -> Maybe [Bool]
    checkNode t val node path
      | node == 0 = if val == 0 then Just path else Nothing
      | node < 0 = if fromIntegral (negate node) == val then Just path else Nothing
      | otherwise =
          let baseIdx = fromIntegral node
           in if baseIdx + 1 < VU.length t
                then findPathToPair t val baseIdx (baseIdx + 1) path
                else Nothing

    writePath :: [Bool] -> VU.Vector Word8 -> Int -> BoolEncoder -> BoolEncoder
    writePath [] _ _ e = e
    writePath (bit : rest) ps probIdx e =
      let prob = ps VU.! probIdx
          e' = boolWrite prob bit e
       in writePath rest ps (probIdx + 1) e'
