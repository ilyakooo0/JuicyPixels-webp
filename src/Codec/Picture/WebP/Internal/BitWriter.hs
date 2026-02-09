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

-- Performance: INLINE pragmas on all hot-path functions

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
{-# INLINE writeBit #-}
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
-- Optimized to batch bits into the buffer when possible
{-# INLINE writeBits #-}
writeBits :: Int -> Word64 -> BitWriter -> BitWriter
writeBits !n !value (BitWriter builder buffer count)
  | n <= 0 = BitWriter builder buffer count
  | otherwise =
      -- Calculate how many bits we can fit in current buffer before needing to flush
      let !remaining = 64 - count -- bits remaining in buffer
       in if n <= remaining
            then
              -- Fast path: all bits fit without flushing
              let !buffer' = buffer .|. (value `shiftL` count)
                  !count' = count + n
               in flushComplete (BitWriter builder buffer' count')
            else
              -- Slow path: need to flush during write
              writeBitsSlow n value (BitWriter builder buffer count)

-- | Flush complete bytes from buffer
{-# INLINE flushComplete #-}
flushComplete :: BitWriter -> BitWriter
flushComplete (BitWriter builder buffer count)
  | count >= 8 =
      let !byte = fromIntegral (buffer .&. 0xFF) :: Word8
          !builder' = builder <> BB.word8 byte
          !buffer' = buffer `shiftR` 8
          !count' = count - 8
       in flushComplete (BitWriter builder' buffer' count')
  | otherwise = BitWriter builder buffer count

-- | Slow path for writeBits when flushing is needed
writeBitsSlow :: Int -> Word64 -> BitWriter -> BitWriter
writeBitsSlow 0 _ writer = writer
writeBitsSlow !numBits !value !writer =
  let !bit = (value .&. 1) /= 0
      !value' = value `shiftR` 1
      !writer' = writeBit bit writer
   in writeBitsSlow (numBits - 1) value' writer'

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
{-# INLINE writeBitsReversed #-}
writeBitsReversed :: Int -> Word64 -> BitWriter -> BitWriter
writeBitsReversed 0 _ writer = writer
writeBitsReversed numBits value writer =
  -- Reverse the bits: write MSB first
  let reversed = reverseBits numBits value
   in writeBits numBits reversed writer

-- | Reverse the bit order of a value
{-# INLINE reverseBits #-}
reverseBits :: Int -> Word64 -> Word64
reverseBits numBits value = go numBits value 0
  where
    go 0 _ acc = acc
    go n v acc = go (n - 1) (v `shiftR` 1) ((acc `shiftL` 1) .|. (v .&. 1))
