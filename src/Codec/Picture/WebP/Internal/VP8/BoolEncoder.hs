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
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Int
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Boolean arithmetic encoder (range coder)
-- Maintains range in [128, 255] and encodes bits with given probabilities
data BoolEncoder = BoolEncoder
  { beBuilder :: !BB.Builder, -- Accumulated output bytes
    beRange :: !Word32, -- Current range [128..255]
    beValue :: !Word32, -- Current coded value
    beCount :: !Int -- Number of bits shifted into current byte (0-7)
  }

-- | Initialize a fresh boolean encoder
-- The first two bytes are reserved for the initial value
initBoolEncoder :: BoolEncoder
initBoolEncoder =
  BoolEncoder
    { beBuilder = BB.word8 0 <> BB.word8 0, -- Reserve space for initial value bytes
      beRange = 255,
      beValue = 0,
      beCount = 0
    }

-- | Write a single bit with given probability
-- prob is in [1, 255], represents probability of 0 as prob/256
boolWrite :: Word8 -> Bool -> BoolEncoder -> BoolEncoder
boolWrite !prob !bit !enc =
  let !split = 1 + (((beRange enc - 1) * fromIntegral prob) `shiftR` 8)
      (!newRange, !newValue) =
        if bit
          then (beRange enc - split, beValue enc + (split `shiftL` 8))
          else (split, beValue enc)
   in renormalize enc {beRange = newRange, beValue = newValue}

-- | Renormalize the encoder state
-- Shifts range and value left while range < 128, outputting bytes as needed
renormalize :: BoolEncoder -> BoolEncoder
renormalize !enc
  | beRange enc >= 128 = enc
  | otherwise =
      let !newRange = beRange enc `shiftL` 1
          !newValue = beValue enc `shiftL` 1
          !newCount = beCount enc + 1
       in if newCount == 8
            then
              let !byte = fromIntegral ((newValue `shiftR` 8) .&. 0xFF) :: Word8
                  !enc' =
                    enc
                      { beBuilder = beBuilder enc <> BB.word8 byte,
                        beRange = newRange,
                        beValue = newValue .&. 0xFF,
                        beCount = 0
                      }
               in renormalize enc'
            else renormalize enc {beRange = newRange, beValue = newValue, beCount = newCount}

-- | Write an n-bit unsigned literal value (MSB-first)
-- Uses probability 128 (equiprobable) for each bit
boolWriteLiteral :: Int -> Word32 -> BoolEncoder -> BoolEncoder
boolWriteLiteral 0 _ !enc = enc
boolWriteLiteral !n !value !enc =
  let !bitPos = n - 1
      !bit = testBit value bitPos
      !enc' = boolWrite 128 bit enc
   in boolWriteLiteral (n - 1) value enc'

-- | Write a signed literal value (magnitude + sign bit)
boolWriteSigned :: Int -> Int32 -> BoolEncoder -> BoolEncoder
boolWriteSigned !n !value !enc =
  let !absValue = abs value
      !enc' = boolWriteLiteral n (fromIntegral absValue) enc
      !sign = value < 0
   in boolWrite 128 sign enc'

-- | Write a value using a binary tree and probabilities
-- tree contains branch structure, probs contains probability for each node
-- value is the leaf token to encode
--
-- VP8 tree format: The tree is stored as pairs of nodes.
-- Traversal starts at pair (0,1) - read a bit to choose index 0 or 1.
-- If the chosen index contains <=0, it's a leaf with value negate(node).
-- If >0, it's the base index of the next pair: (base, base+1).
boolWriteTree :: VU.Vector Int8 -> VU.Vector Word8 -> Int -> BoolEncoder -> BoolEncoder
boolWriteTree !tree !probs !targetValue !enc =
  case findPathToPair tree targetValue 0 1 [] of
    Just path -> writePath (reverse path) probs 0 enc
    Nothing -> error $ "VP8 encoder error: value " ++ show targetValue ++ " not found in tree"
  where
    -- Find path starting from a pair of indices (leftIdx, rightIdx)
    -- Returns the sequence of bits (False=left, True=right) to reach the target
    findPathToPair :: VU.Vector Int8 -> Int -> Int -> Int -> [Bool] -> Maybe [Bool]
    findPathToPair t val leftIdx rightIdx path
      | leftIdx >= VU.length t || rightIdx >= VU.length t = Nothing
      | otherwise =
          let leftNode = t VU.! leftIdx
              rightNode = t VU.! rightIdx
           in -- Try left (bit=0)
              case checkNode t val leftNode (False : path) of
                Just p -> Just p
                Nothing ->
                  -- Try right (bit=1)
                  checkNode t val rightNode (True : path)

    -- Check if a node matches or leads to the target
    checkNode :: VU.Vector Int8 -> Int -> Int8 -> [Bool] -> Maybe [Bool]
    checkNode t val node path
      | node == 0 =
          -- Special case: 0 represents token 0 (DCT_0)
          if val == 0
            then Just path
            else Nothing
      | node < 0 =
          -- Leaf - check if it matches
          if fromIntegral (negate node) == val
            then Just path
            else Nothing
      | otherwise =
          -- Branch - continue with next pair
          let baseIdx = fromIntegral node
           in if baseIdx + 1 < VU.length t
                then findPathToPair t val baseIdx (baseIdx + 1) path
                else Nothing

    -- Write the path bits using probabilities
    writePath :: [Bool] -> VU.Vector Word8 -> Int -> BoolEncoder -> BoolEncoder
    writePath [] _ _ e = e
    writePath (bit : rest) ps probIdx e =
      let prob = ps VU.! probIdx
          e' = boolWrite prob bit e
       in writePath rest ps (probIdx + 1) e'

-- | Finalize the encoder and return the encoded bytestring
-- Flushes any remaining bits and returns the complete encoded data
finalizeBoolEncoder :: BoolEncoder -> B.ByteString
finalizeBoolEncoder !enc =
  -- Flush remaining bits by shifting value to get final bytes
  let !finalValue = beValue enc
      -- We need to output remaining bits
      -- Shift value to align to byte boundary and output
      !remainingBits = beCount enc
      !enc' =
        if remainingBits > 0
          then
            let !byte = fromIntegral ((finalValue `shiftR` 8) .&. 0xFF) :: Word8
             in enc {beBuilder = beBuilder enc <> BB.word8 byte}
          else enc
      -- Output final padding if needed
      !finalBuilder = beBuilder enc'
   in BL.toStrict $ BB.toLazyByteString finalBuilder
