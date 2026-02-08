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

      w =
        emptyBitWriter
          |> writeBits 8 0x2F -- Signature
          |> writeBits 14 (fromIntegral $ width - 1)
          |> writeBits 14 (fromIntegral $ height - 1)
          |> writeBit True -- alpha_is_used
          |> writeBits 3 0 -- version
          |> writeBit False -- no transforms
          |> writeBit False -- no color cache
          |> writeBit False -- single prefix code group
          |> writeUncompressedCodes
          |> writeUncompressedPixels pixels (width * height)
          |> finalizeBitWriter
   in bitWriterToByteString w
  where
    (|>) = flip ($)

-- | Write uncompressed prefix codes
writeUncompressedCodes :: BitWriter -> BitWriter
writeUncompressedCodes w =
  -- Write 5 codes: Green(256), Red(256), Blue(256), Alpha(256), Distance(40)
  w
    |> writeUncompressed256 -- Green
    |> writeUncompressed256 -- Red
    |> writeUncompressed256 -- Blue
    |> writeUncompressed256 -- Alpha
    |> writeSimpleCode 0 -- Distance: simple code (no back-refs in uncompressed)
  where
    (|>) = flip ($)

-- | Write code with all 256 symbols having length 8
writeUncompressed256 :: BitWriter -> BitWriter
writeUncompressed256 w =
  let w1 = writeBit False w -- is_simple = 0

      -- Strategy: Use symbol 8 (literal 8) and symbol 16 (repeat previous)
      -- kCodeLengthCodeOrder = [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, ...]
      -- Symbol 8 at position 11, symbol 16 at position 8

      -- Write CLC with symbols 8 and 16 having length 1
      numCLC = 12
      w2 = writeBits 4 (numCLC - 4) w1 -- = 8, so 4+8=12 code lengths

      -- Positions 0-11 in kCodeLengthCodeOrder: [17,18,0,1,2,3,4,5,16,6,7,8]
      -- Give symbol 16 (pos 8) and symbol 8 (pos 11) length 1, others 0
      w3 = writeBits 3 0 w2 -- pos 0: sym 17, len 0
      w4 = writeBits 3 0 w3 -- pos 1: sym 18, len 0
      w5 = writeBits 3 0 w4 -- pos 2: sym 0, len 0
      w6 = writeBits 3 0 w5 -- pos 3: sym 1, len 0
      w7 = writeBits 3 0 w6 -- pos 4: sym 2, len 0
      w8 = writeBits 3 0 w7 -- pos 5: sym 3, len 0
      w9 = writeBits 3 0 w8 -- pos 6: sym 4, len 0
      w10 = writeBits 3 0 w9 -- pos 7: sym 5, len 0
      w11 = writeBits 3 1 w10 -- pos 8: sym 16, len 1 ✓
      w12 = writeBits 3 0 w11 -- pos 9: sym 6, len 0
      w13 = writeBits 3 0 w12 -- pos 10: sym 7, len 0
      w14 = writeBits 3 1 w13 -- pos 11: sym 8, len 1 ✓

      -- use_max_symbol = 0 (all 256)
      w15 = writeBit False w14

      -- Write 256 code lengths using the CLC
      -- Canonical codes for CLC: symbol 16 < symbol 8, so: 16→code 0, 8→code 1
      -- Write: symbol 8 (literal "8"), then symbol 16 with extra bits to repeat
      -- Symbol 8 = code 1 = bit 1
      w16 = writeBit True w15

      -- Now repeat using symbol 16 (code 0, repeat previous 3-6 times)
      -- We need 255 more copies of length 8
      -- Symbol 16 can repeat 3-6 times with 2 extra bits
      -- Use max repeat (6) as many times as needed

      -- Helper to write one repeat-6
      writeRepeat6 wa = writeBit False wa |> writeBits 2 3
        where
          (|>) = flip ($)

      -- Write 42 repeat-6 operations (42*6 = 252)
      w17 = foldl (\wa _ -> writeRepeat6 wa) w16 [1 .. 42]

      -- Write final repeat-3 (255 - 252 = 3 remaining)
      w18 = writeBit False w17 |> writeBits 2 0 -- repeat 3 times (0+3=3)
        where
          (|>) = flip ($)
   in w18

-- | Write simple 1-symbol code
writeSimpleCode :: Word16 -> BitWriter -> BitWriter
writeSimpleCode sym w =
  writeBit True w |> writeBit False |> writeBit True |> writeBits 8 (fromIntegral sym)
  where
    (|>) = flip ($)

-- | Write pixels uncompressed (8 bits per channel)
writeUncompressedPixels :: VS.Vector Word8 -> Int -> BitWriter -> BitWriter
writeUncompressedPixels pixels numPixels w =
  foldl
    ( \wa i ->
        let r = pixels VS.! (i * 4)
            g = pixels VS.! (i * 4 + 1)
            b = pixels VS.! (i * 4 + 2)
            a = pixels VS.! (i * 4 + 3)
         in wa
              |> writeBits 8 (fromIntegral g) -- Green first in VP8L
              |> writeBits 8 (fromIntegral r)
              |> writeBits 8 (fromIntegral b)
              |> writeBits 8 (fromIntegral a)
    )
    w
    [0 .. numPixels - 1]
  where
    (|>) = flip ($)
