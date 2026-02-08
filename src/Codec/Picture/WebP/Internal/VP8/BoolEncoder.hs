{-# LANGUAGE BangPatterns #-}

-- | VP8 Boolean Arithmetic Encoder
-- Based on libwebp's VP8BitWriter implementation
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
-- Based on libwebp's VP8BitWriter with proper carry propagation
data BoolEncoder = BoolEncoder
  { beBytes :: ![Word8], -- Output bytes in reverse order
    beRange :: !Int32,   -- Range [0..254], stores range-1 per libwebp convention
    beValue :: !Int32,   -- Accumulated bits (can grow > 255)
    beRun :: !Int,       -- Pending 0xff bytes (for carry propagation)
    beNbBits :: !Int     -- Pending bits count (starts at -8)
  }

-- | kNorm table: number of bits to shift (8 - log2(range))
kNorm :: VU.Vector Int
kNorm = VU.fromList
  [ 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3
  , 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
  , 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1
  , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0
  ]

-- | kNewRange table: range = ((range + 1) << shift) - 1
kNewRange :: VU.Vector Int32
kNewRange = VU.fromList
  [ 127, 127, 191, 127, 159, 191, 223, 127, 143, 159, 175, 191, 207, 223, 239
  , 127, 135, 143, 151, 159, 167, 175, 183, 191, 199, 207, 215, 223, 231, 239
  , 247, 127, 131, 135, 139, 143, 147, 151, 155, 159, 163, 167, 171, 175, 179
  , 183, 187, 191, 195, 199, 203, 207, 211, 215, 219, 223, 227, 231, 235, 239
  , 243, 247, 251, 127, 129, 131, 133, 135, 137, 139, 141, 143, 145, 147, 149
  , 151, 153, 155, 157, 159, 161, 163, 165, 167, 169, 171, 173, 175, 177, 179
  , 181, 183, 185, 187, 189, 191, 193, 195, 197, 199, 201, 203, 205, 207, 209
  , 211, 213, 215, 217, 219, 221, 223, 225, 227, 229, 231, 233, 235, 237, 239
  , 241, 243, 245, 247, 249, 251, 253, 127
  ]

-- | Initialize a boolean encoder
initBoolEncoder :: BoolEncoder
initBoolEncoder =
  BoolEncoder
    { beBytes = [],
      beRange = 254, -- range-1, like libwebp
      beValue = 0,
      beRun = 0,
      beNbBits = -8  -- Start with 8 bits buffered
    }

-- | Flush bits from value to output bytes with carry propagation
flush :: BoolEncoder -> BoolEncoder
flush !enc =
  let !s = 8 + beNbBits enc
      !bits = beValue enc `shiftR` s
      !newValue = beValue enc - (bits `shiftL` s)
      !newNbBits = beNbBits enc - 8
  in if (bits .&. 0xff) /= 0xff
       then
         -- Normal case: output the byte with potential carry
         let !hasCarry = (bits .&. 0x100) /= 0
             !outputByte = fromIntegral (bits .&. 0xff) :: Word8
             -- Handle carry: increment previous byte if needed
             !bytes1 = if hasCarry && not (null (beBytes enc))
                         then incrementLast (beBytes enc)
                         else beBytes enc
             -- Flush pending 0xff bytes
             !flushValue = if hasCarry then 0x00 else 0xff
             !bytes2 = flushRun (beRun enc) flushValue bytes1
             -- Add current byte
             !bytes3 = outputByte : bytes2
         in enc { beBytes = bytes3, beValue = newValue, beNbBits = newNbBits, beRun = 0 }
       else
         -- Byte is 0xff: delay output (might need carry propagation)
         enc { beValue = newValue, beNbBits = newNbBits, beRun = beRun enc + 1 }
  where
    incrementLast (x:xs) = (x + 1) : xs
    incrementLast [] = []

    flushRun 0 _ bytes = bytes
    flushRun n val bytes = flushRun (n - 1) val (val : bytes)

-- | Write one bit with given probability
-- Probability is [1..255] where 128 means 50/50
boolWrite :: Word8 -> Bool -> BoolEncoder -> BoolEncoder
boolWrite !prob !bit !enc =
  let !range = beRange enc
      !split = (range * fromIntegral prob) `shiftR` 8
      (!newRange, !newValue) =
        if bit
          then (range - split - 1, beValue enc + split + 1)
          else (split, beValue enc)
  in if newRange < 127
       then
         -- Renormalize using lookup tables
         let !rangeIdx = fromIntegral newRange :: Int
             !shift = kNorm VU.! rangeIdx
             !finalRange = kNewRange VU.! rangeIdx
             !shiftedValue = newValue `shiftL` shift
             !newNbBits = beNbBits enc + shift
             !enc' = enc { beRange = finalRange, beValue = shiftedValue, beNbBits = newNbBits }
         in if newNbBits > 0
              then flush enc'
              else enc'
       else
         enc { beRange = newRange }

-- | Write one bit with uniform (50/50) probability
boolWriteUniform :: Bool -> BoolEncoder -> BoolEncoder
boolWriteUniform !bit !enc =
  let !range = beRange enc
      !split = range `shiftR` 1
      (!newRange, !newValue) =
        if bit
          then (range - split - 1, beValue enc + split + 1)
          else (split, beValue enc)
  in if newRange < 127
       then
         let !rangeIdx = fromIntegral newRange :: Int
             !shift = kNorm VU.! rangeIdx
             !finalRange = kNewRange VU.! rangeIdx
             !shiftedValue = newValue `shiftL` shift
             !newNbBits = beNbBits enc + shift
             !enc' = enc { beRange = finalRange, beValue = shiftedValue, beNbBits = newNbBits }
         in if newNbBits > 0
              then flush enc'
              else enc'
       else
         enc { beRange = newRange }

-- | Write n bits as a literal value (MSB first, uniform probability)
boolWriteLiteral :: Int -> Word32 -> BoolEncoder -> BoolEncoder
boolWriteLiteral 0 _ !enc = enc
boolWriteLiteral !n !value !enc =
  let !bit = testBit value (n - 1)
  in boolWriteLiteral (n - 1) value (boolWriteUniform bit enc)

-- | Write a signed value: magnitude first, then sign
boolWriteSigned :: Int -> Int32 -> BoolEncoder -> BoolEncoder
boolWriteSigned !n !value !enc =
  let !magnitude = fromIntegral (abs value) :: Word32
      !enc' = boolWriteLiteral n magnitude enc
  in boolWrite 128 (value < 0) enc'

-- | Finalize the encoder and return the encoded bytes
finalizeBoolEncoder :: BoolEncoder -> B.ByteString
finalizeBoolEncoder !enc =
  let -- Write padding bits to flush remaining data
      -- We need to write enough bits to ensure the decoder can read our data
      -- libwebp uses: VP8PutBits(bw, 0, 9 - bw->nb_bits)
      !padBits = max 0 (9 - beNbBits enc)
      !enc' = writePadding padBits enc
      -- Flush any remaining bytes
      !enc'' = finalFlush enc'
      !finalBytes = reverse (beBytes enc'')
  in B.pack finalBytes
  where
    writePadding 0 e = e
    writePadding n e = writePadding (n - 1) (boolWriteUniform False e)

    -- Final flush to output any remaining buffered data
    finalFlush e
      | beNbBits e > 0 = finalFlush (flush e)
      | beRun e > 0 =
          -- Output pending 0xff bytes
          let bytes' = flushRun (beRun e) 0xff (beBytes e)
          in e { beBytes = bytes', beRun = 0 }
      | otherwise = e

    flushRun 0 _ bytes = bytes
    flushRun n val bytes = flushRun (n - 1) val (val : bytes)

-- | Write a value using a tree structure and probability table
-- This matches the RFC 6386 tree format where:
-- - Tree is structured as pairs: indices (i, i+1) are left/right children
-- - We write a bit (False=left, True=right) to select which child
-- - Negative values are leaves (-token), positive values are branch indices
boolWriteTree :: VU.Vector Int8 -> VU.Vector Word8 -> Int -> BoolEncoder -> BoolEncoder
boolWriteTree !tree !probs !targetValue !enc =
  case findPath 0 0 [] of
    Just path -> writePath (reverse path) probs 0 enc
    Nothing -> error $ "VP8 encoder: value " ++ show targetValue ++ " not in tree"
  where
    -- Find path to target value starting at tree position i
    -- i is the base index for the current pair (i, i+1)
    findPath !i !depth !path
      | i >= VU.length tree || i + 1 >= VU.length tree = Nothing
      | otherwise =
          let leftNode = tree VU.! i
              rightNode = tree VU.! (i + 1)
          in -- Check left child (bit = False)
             case checkNode leftNode (False : path) of
               Just p -> Just p
               Nothing ->
                 -- Check right child (bit = True)
                 checkNode rightNode (True : path)
      where
        checkNode node path'
          | node == 0 = if targetValue == 0 then Just path' else Nothing
          | node < 0 = if fromIntegral (negate node) == targetValue then Just path' else Nothing
          | otherwise = findPath (fromIntegral node) (depth + 1) path'

    writePath [] _ _ e = e
    writePath (bit : rest) ps probIdx e =
      writePath rest ps (probIdx + 1) (boolWrite (ps VU.! probIdx) bit e)
