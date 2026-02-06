{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.BoolDecoder
  ( BoolDecoder
  , initBoolDecoder
  , boolRead
  , boolLiteral
  , boolSigned
  , boolReadTree
  )
where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Data.Int

-- | Boolean arithmetic decoder for VP8
-- Range is maintained in [128..255]
-- This is a hot path - called millions of times per image
data BoolDecoder = BoolDecoder
  { bdBytes :: !B.ByteString
  , bdRange :: !Word32
  , bdValue :: !Word32
  , bdCount :: !Int
  , bdPos :: !Int
  }
  deriving (Show)

-- | Initialize a boolean decoder from a ByteString
initBoolDecoder :: B.ByteString -> BoolDecoder
initBoolDecoder bs
  | B.length bs < 2 = error "BoolDecoder: need at least 2 bytes"
  | otherwise =
      let byte0 = fromIntegral (B.index bs 0) :: Word32
          byte1 = fromIntegral (B.index bs 1) :: Word32
          value = (byte0 `shiftL` 8) .|. byte1
       in BoolDecoder
            { bdBytes = bs
            , bdRange = 255
            , bdValue = value
            , bdCount = 0
            , bdPos = 2
            }

-- | Read a single bit with given probability
-- Probability is in range [1..255], where 128 means 50/50
boolRead :: Word8 -> BoolDecoder -> (Bool, BoolDecoder)
boolRead prob decoder@(BoolDecoder bytes range value count pos) =
  let split = 1 + (((range - 1) * fromIntegral prob) `shiftR` 8)
      bigSplit = split `shiftL` 8
   in if value >= bigSplit
        then
          let newRange = range - split
              newValue = value - bigSplit
              (finalRange, finalValue, finalCount, finalPos) =
                renormalize bytes newRange newValue count pos
           in ( True
              , BoolDecoder bytes finalRange finalValue finalCount finalPos
              )
        else
          let (finalRange, finalValue, finalCount, finalPos) =
                renormalize bytes split value count pos
           in ( False
              , BoolDecoder bytes finalRange finalValue finalCount finalPos
              )

-- | Read n bits as a literal (probability 128, MSB-first)
boolLiteral :: Int -> BoolDecoder -> (Word32, BoolDecoder)
boolLiteral n decoder = go 0 n decoder
  where
    go !acc !remaining !d
      | remaining <= 0 = (acc, d)
      | otherwise =
          let (bit, d') = boolRead 128 d
              acc' = (acc `shiftL` 1) .|. if bit then 1 else 0
           in go acc' (remaining - 1) d'

-- | Read a signed value: n-bit magnitude + sign bit
boolSigned :: Int -> BoolDecoder -> (Int32, BoolDecoder)
boolSigned n decoder =
  let (magnitude, decoder1) = boolLiteral n decoder
      (sign, decoder2) = boolRead 128 decoder1
      value = if sign then -(fromIntegral magnitude) else fromIntegral magnitude
   in (value, decoder2)

-- | Read a symbol using a tree and probability table
-- Tree format: negative values are leaf symbols, positive values are node indices
-- Returns the symbol (as an Int, not the tree value)
boolReadTree :: V.Vector Int8 -> V.Vector Word8 -> BoolDecoder -> (Int, BoolDecoder)
boolReadTree tree probs decoder = go 0 decoder
  where
    go !i !d =
      let node = tree V.! i
       in if node <= 0
            then (fromIntegral (negate node), d)
            else
              let prob = probs V.! (i `shiftR` 1)
                  (bit, d') = boolRead prob d
                  nextIdx = fromIntegral node + if bit then 1 else 0
               in go nextIdx d'

-- | Renormalize the range to keep it in [128..255]
renormalize :: B.ByteString -> Word32 -> Word32 -> Int -> Int -> (Word32, Word32, Int, Int)
renormalize bytes range value count pos = go range value count pos
  where
    go !r !v !c !p
      | r >= 128 = (r, v, c, p)
      | c == 8 =
          let newByte =
                if p < B.length bytes
                  then fromIntegral (B.index bytes p)
                  else 0
              v' = (v `shiftL` 8) .|. newByte
              r' = r `shiftL` 8
           in go r' v' 0 (p + 1)
      | otherwise =
          let v' = v `shiftL` 1
              r' = r `shiftL` 1
           in go r' v' (c + 1) p
