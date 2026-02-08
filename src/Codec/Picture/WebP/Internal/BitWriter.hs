{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.BitWriter
  ( BitWriter,
    emptyBitWriter,
    writeBit,
    writeBits,
    writeBitsReversed,
    finalizeBitWriter,
    bitWriterToByteString,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Word

-- | Bit writer for LSB-first bit packing (VP8L style)
data BitWriter = BitWriter
  { bwBuilder :: !BB.Builder,
    bwBuffer :: !Word64,
    bwBitCount :: !Int
  }

-- | Create an empty bit writer
emptyBitWriter :: BitWriter
emptyBitWriter = BitWriter mempty 0 0

-- | Write a single bit (LSB first)
writeBit :: Bool -> BitWriter -> BitWriter
writeBit bit (BitWriter builder buffer count) =
  let buffer' = buffer .|. (if bit then 1 `shiftL` count else 0)
      count' = count + 1
   in if count' >= 8
        then
          let byte = fromIntegral (buffer' .&. 0xFF) :: Word8
              builder' = builder <> BB.word8 byte
              buffer'' = buffer' `shiftR` 8
              count'' = count' - 8
           in BitWriter builder' buffer'' count''
        else BitWriter builder buffer' count'

-- | Write multiple bits (LSB first)
writeBits :: Int -> Word64 -> BitWriter -> BitWriter
writeBits 0 _ writer = writer
writeBits numBits value writer =
  let bit = (value .&. 1) /= 0
      value' = value `shiftR` 1
      writer' = writeBit bit writer
   in writeBits (numBits - 1) value' writer'

-- | Finalize bit writer by padding to byte boundary
finalizeBitWriter :: BitWriter -> BitWriter
finalizeBitWriter writer@(BitWriter builder buffer count)
  | count == 0 = writer
  | otherwise =
      let byte = fromIntegral (buffer .&. 0xFF) :: Word8
          builder' = builder <> BB.word8 byte
       in BitWriter builder' 0 0

-- | Convert bit writer to ByteString
bitWriterToByteString :: BitWriter -> B.ByteString
bitWriterToByteString (BitWriter builder _ _) =
  B.toStrict $ BB.toLazyByteString builder

-- | Write bits in reversed order (MSB first)
-- Used for Huffman codes where the first-read bit is the MSB of the code
writeBitsReversed :: Int -> Word64 -> BitWriter -> BitWriter
writeBitsReversed 0 _ writer = writer
writeBitsReversed numBits value writer =
  -- Reverse the bits: write MSB first
  let reversed = reverseBits numBits value
   in writeBits numBits reversed writer

-- | Reverse the bit order of a value
reverseBits :: Int -> Word64 -> Word64
reverseBits numBits value = go numBits value 0
  where
    go 0 _ acc = acc
    go n v acc = go (n - 1) (v `shiftR` 1) ((acc `shiftL` 1) .|. (v .&. 1))
