{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.BitReader
  ( BitReader,
    initBitReader,
    readBits,
    readBit,
    getBytesRemaining,
    getBitPosition,
    getReaderState,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
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
-- Pre-fills buffer to 56+ bits for optimal performance
{-# INLINE initBitReader #-}
initBitReader :: B.ByteString -> BitReader
initBitReader bs =
  let reader = BitReader bs 0 0 0
   in refillBufferFull reader

-- | Read n bits (n <= 32) from the bitstream
-- Returns the bits as Word32 and the updated reader
-- Bits are returned LSB-first: the first bit read is at position 0
-- If not enough bits are available, returns 0s for missing bits
{-# INLINE readBits #-}
readBits :: Int -> BitReader -> (Word32, BitReader)
readBits !n reader@(BitReader bytes offset bits count)
  | n == 0 = (0, reader)
  | count >= n =
      -- Fast path: enough bits in buffer
      let !mask = (1 `shiftL` n) - 1
          !result = fromIntegral (bits .&. mask)
          !newBits = bits `shiftR` n
          !newCount = count - n
       in (result, BitReader bytes offset newBits newCount)
  | otherwise =
      -- Need to refill
      let !reader'@(BitReader _ _ _ count') = refillBufferFull reader
       in if count' >= n
            then readBits n reader'
            else
              -- Can't refill enough, return what we have padded with zeros
              let !mask = (1 `shiftL` count') - 1
                  !result = fromIntegral (brBits reader' .&. mask)
               in (result, BitReader bytes (brOffset reader') 0 0)

-- | Read a single bit from the bitstream (specialized fast path)
{-# INLINE readBit #-}
readBit :: BitReader -> (Bool, BitReader)
readBit reader@(BitReader bytes offset bits count)
  | count >= 1 =
      -- Fast path: have at least 1 bit
      let !result = (bits .&. 1) == 1
          !newBits = bits `shiftR` 1
          !newCount = count - 1
       in (result, BitReader bytes offset newBits newCount)
  | otherwise =
      -- Need to refill first
      let !reader' = refillBufferFull reader
          (val, reader'') = readBits 1 reader'
       in (val /= 0, reader'')

-- | Get remaining bytes in the reader (for debugging)
getBytesRemaining :: BitReader -> Int
getBytesRemaining (BitReader bytes offset _ _) =
  B.length bytes - offset

-- | Get approximate bit position (for debugging)
-- Returns the number of bits consumed from the start
getBitPosition :: BitReader -> Int
getBitPosition (BitReader _ offset _ count) =
  offset * 8 - count

-- | Get reader state as a string (for debugging)
getReaderState :: BitReader -> String
getReaderState (BitReader _ offset bits count) =
  "offset="
    ++ show offset
    ++ ", bits=0x"
    ++ showHex64 bits
    ++ ", count="
    ++ show count
    ++ ", pos="
    ++ show (offset * 8 - count)
  where
    showHex64 :: Word64 -> String
    showHex64 w = concatMap toHexDigit [60, 56 .. 0]
      where
        toHexDigit shift = [hexChars !! fromIntegral ((w `shiftR` shift) .&. 0xF)]
        hexChars = "0123456789abcdef"

-- | Refill the bit buffer to at least 32 bits (if possible)
-- Uses batch byte reading for efficiency
{-# INLINE refillBuffer #-}
refillBuffer :: BitReader -> BitReader
refillBuffer reader@(BitReader bytes offset bits count)
  | count >= 32 = reader
  | offset >= B.length bytes = reader
  | otherwise =
      let !bytesAvailable = B.length bytes - offset
          -- Calculate how many bytes we can add without exceeding 64 bits total
          !maxBitsToAdd = 64 - count
          !maxBytesToAdd = maxBitsToAdd `shiftR` 3  -- div 8
          !bytesToRead = min maxBytesToAdd (min 8 bytesAvailable)
       in if bytesToRead > 0
            then
              let !newBytes = readBytesLEFast bytes offset bytesToRead
                  !newBits = bits .|. (newBytes `shiftL` count)
                  !newCount = count + (bytesToRead `shiftL` 3)  -- * 8
                  !newOffset = offset + bytesToRead
               in BitReader bytes newOffset newBits newCount
            else reader

-- | Aggressively refill the buffer to 56+ bits for optimal performance
-- Called at initialization and when buffer runs low
{-# INLINE refillBufferFull #-}
refillBufferFull :: BitReader -> BitReader
refillBufferFull reader@(BitReader bytes offset bits count)
  | count >= 56 = reader
  | offset >= B.length bytes = reader
  | otherwise =
      let !bytesAvailable = B.length bytes - offset
          !maxBitsToAdd = 64 - count
          !maxBytesToAdd = maxBitsToAdd `shiftR` 3
          !bytesToRead = min maxBytesToAdd (min 8 bytesAvailable)
       in if bytesToRead > 0
            then
              let !newBytes = readBytesLEFast bytes offset bytesToRead
                  !newBits = bits .|. (newBytes `shiftL` count)
                  !newCount = count + (bytesToRead `shiftL` 3)
                  !newOffset = offset + bytesToRead
                  !reader' = BitReader bytes newOffset newBits newCount
               in if newCount < 56 && newOffset < B.length bytes
                    then refillBufferFull reader'
                    else reader'
            else reader

-- | Read up to 8 bytes as a little-endian Word64 (optimized)
-- Uses unsafeIndex for speed in hot path
{-# INLINE readBytesLEFast #-}
readBytesLEFast :: B.ByteString -> Int -> Int -> Word64
readBytesLEFast bytes offset n = go 0 0
  where
    go !acc !i
      | i >= n = acc
      | otherwise =
          let !byte = fromIntegral (BU.unsafeIndex bytes (offset + i))
              !acc' = acc .|. (byte `shiftL` (i `shiftL` 3))  -- i * 8
           in go acc' (i + 1)
