{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.BitReader
  ( BitReader,
    initBitReader,
    readBits,
    readBit,
    getBytesRemaining,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import Data.Word

-- | LSB-first bit reader for VP8L
-- This is a hot path - called millions of times per image
data BitReader = BitReader
  { brBytes :: !B.ByteString,
    brOffset :: !Int,
    brBits :: !Word64,
    brCount :: !Int
  }
  deriving (Show)

-- | Initialize a bit reader from a ByteString
initBitReader :: B.ByteString -> BitReader
initBitReader bs =
  let reader = BitReader bs 0 0 0
   in refillBuffer reader

-- | Read n bits (n <= 32) from the bitstream
-- Returns the bits as Word32 and the updated reader
-- Bits are returned LSB-first: the first bit read is at position 0
-- If not enough bits are available, returns 0s for missing bits
readBits :: Int -> BitReader -> (Word32, BitReader)
readBits n reader@(BitReader bytes offset bits count)
  | n < 0 || n > 32 = error "readBits: n must be in [0, 32]"
  | n == 0 = (0, reader)
  | count < n =
      let reader'@(BitReader _ _ _ count') = refillBuffer reader
       in if count' > count
            then readBits n reader' -- Made progress, try again
            else
              -- Can't refill more, return what we have padded with zeros
              let mask = (1 `shiftL` count) - 1
                  result = fromIntegral (bits .&. mask)
               in (result, BitReader bytes offset 0 0)
  | otherwise =
      let mask = (1 `shiftL` n) - 1
          result = fromIntegral (bits .&. mask)
          newBits = bits `shiftR` n
          newCount = count - n
       in (result, BitReader bytes offset newBits newCount)

-- | Read a single bit from the bitstream
readBit :: BitReader -> (Bool, BitReader)
readBit reader =
  let (val, reader') = readBits 1 reader
   in (val /= 0, reader')

-- | Get remaining bytes in the reader (for debugging)
getBytesRemaining :: BitReader -> Int
getBytesRemaining (BitReader bytes offset _ _) =
  B.length bytes - offset

-- | Refill the bit buffer when it has less than 32 bits
-- Reads up to 8 bytes to fill the 64-bit buffer
refillBuffer :: BitReader -> BitReader
refillBuffer reader@(BitReader bytes offset bits count)
  | count >= 32 = reader
  | offset >= B.length bytes = reader
  | otherwise =
      let bytesAvailable = B.length bytes - offset
          bytesToRead = min 8 bytesAvailable
          newBytes = readBytesLE bytes offset bytesToRead
          newBits = bits .|. (newBytes `shiftL` count)
          newCount = count + (bytesToRead * 8)
          newOffset = offset + bytesToRead
       in BitReader bytes newOffset newBits newCount

-- | Read up to 8 bytes as a little-endian Word64
readBytesLE :: B.ByteString -> Int -> Int -> Word64
readBytesLE bytes offset n = go 0 0
  where
    go !acc !i
      | i >= n = acc
      | otherwise =
          let byte = fromIntegral (B.index bytes (offset + i))
              acc' = acc .|. (byte `shiftL` (i * 8))
           in go acc' (i + 1)
