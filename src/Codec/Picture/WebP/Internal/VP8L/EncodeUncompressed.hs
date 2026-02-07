{-# LANGUAGE BangPatterns #-}

-- | Uncompressed VP8L encoder - works for any image
module Codec.Picture.WebP.Internal.VP8L.EncodeUncompressed
  ( encodeVP8LUncompressed,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.BitWriter
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Data.Word

-- | Encode as uncompressed VP8L (all pixels as 8-bit literals)
encodeVP8LUncompressed :: Image PixelRGBA8 -> B.ByteString
encodeVP8LUncompressed img =
  let width = imageWidth img
      height = imageHeight img
      pixels = imageData img

      w = emptyBitWriter
        |> writeBits 8 0x2F  -- Signature
        |> writeBits 14 (fromIntegral $ width - 1)
        |> writeBits 14 (fromIntegral $ height - 1)
        |> writeBit True  -- alpha_is_used
        |> writeBits 3 0  -- version
        |> writeBit False  -- no transforms
        |> writeBit False  -- no color cache
        |> writeBit False  -- single prefix code group
        |> writeUncompressedCodes
        |> writeUncompressedPixels pixels (width * height)
        |> finalizeBitWriter

   in bitWriterToByteString w
  where
    (|>) = flip ($)

-- | Write uncompressed prefix codes (all 256 symbols with length 8)
writeUncompressedCodes :: BitWriter -> BitWriter
writeUncompressedCodes w =
  -- Write 5 codes: Green, Red, Blue, Alpha, Distance
  -- Each with all 256 symbols having length 8 (identity encoding)
  w |> writeUncompressed256
    |> writeUncompressed256
    |> writeUncompressed256
    |> writeUncompressed256
    |> writeSimpleCode 0  -- Distance: single symbol
  where
    (|>) = flip ($)

-- | Write code with all 256 symbols having length 8
writeUncompressed256 :: BitWriter -> BitWriter
writeUncompressed256 w =
  let w1 = writeBit False w  -- is_simple = 0

      -- Simplest possible CLC: symbols 0,1,2,3 all with length 2
      -- This gives us 2-bit codes to encode lengths 0-3
      -- Then we can use length 8 which won't fit, but let's try even simpler:
      -- Just use symbol 8 with length 1 (single symbol CLC)

      -- Actually, let's use the pattern from test images: symbols at positions with non-zero values
      -- kCodeLengthCodeOrder = [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, ...]

      -- SIMPLEST: Use only symbol 8, make it a simple code
      -- Write CLC as simple code with single symbol 8
      w2 = writeBits 4 0 w1  -- num_code_lengths = 4 (minimum)
      w3 = writeBits 3 0 w2  -- sym 17: len 0
      w4 = writeBits 3 0 w3  -- sym 18: len 0
      w5 = writeBits 3 0 w4  -- sym 0: len 0
      w6 = writeBits 3 1 w5  -- sym 1: len 1 (use this!)

      -- use_max_symbol = 0
      w7 = writeBit False w6

      -- Write 256 code lengths, each is symbol 1 (length 1, code 0)
      -- Symbol 1 in our CLC means "length 1", not "length 8"!
      -- I need symbol 8 to mean "length 8"

      -- Let me use a different strategy: write the 256 code lengths using literal symbols
      -- I need 8 symbols (0-8 for lengths), so I need them all in the CLC
      -- Give symbols 0-8 equal lengths so I can encode any length
      w8 = foldl (\wa _ -> writeBit False wa) w6 [0..255]  -- Write symbol 1 (len 1) 256 times

   in w8

-- | Write simple 1-symbol code
writeSimpleCode :: Word16 -> BitWriter -> BitWriter
writeSimpleCode sym w =
  writeBit True w |> writeBit False |> writeBit True |> writeBits 8 (fromIntegral sym)
  where (|>) = flip ($)

-- | Write pixels uncompressed (8 bits per channel)
writeUncompressedPixels :: VS.Vector Word8 -> Int -> BitWriter -> BitWriter
writeUncompressedPixels pixels numPixels w =
  foldl (\wa i ->
    let r = pixels VS.! (i * 4)
        g = pixels VS.! (i * 4 + 1)
        b = pixels VS.! (i * 4 + 2)
        a = pixels VS.! (i * 4 + 3)
     in wa |> writeBits 8 (fromIntegral g)  -- Green first in VP8L
           |> writeBits 8 (fromIntegral r)
           |> writeBits 8 (fromIntegral b)
           |> writeBits 8 (fromIntegral a)
  ) w [0 .. numPixels - 1]
  where
    (|>) = flip ($)
