{-# LANGUAGE BangPatterns #-}

-- | Identity encoder - all 256 symbols with length 8 (uncompressed but works)
module Codec.Picture.WebP.Internal.VP8L.EncodeIdentity
  ( encodeVP8LIdentity,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.BitWriter
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Data.Word

-- | Encode with identity codes (symbol N → code N in 8 bits)
encodeVP8LIdentity :: Image PixelRGBA8 -> B.ByteString
encodeVP8LIdentity img =
  let width = imageWidth img
      height = imageHeight img
      pixels = imageData img

      w = emptyBitWriter
        |> writeBits 8 0x2F  -- VP8L signature
        |> writeBits 14 (fromIntegral $ width - 1)
        |> writeBits 14 (fromIntegral $ height - 1)
        |> writeBit True  -- alpha_is_used
        |> writeBits 3 0  -- version
        |> writeBit False  -- no transforms
        |> writeBit False  -- no color cache
        |> writeBit False  -- single prefix code group
        |> writeIdentityCode  -- Green
        |> writeIdentityCode  -- Red
        |> writeIdentityCode  -- Blue
        |> writeIdentityCode  -- Alpha
        |> writeSimple1 0     -- Distance
        |> writeAllPixels pixels (width * height)
        |> finalizeBitWriter

   in bitWriterToByteString w
  where
    (|>) = flip ($)

-- | Write identity code: all 256 symbols with length 8
writeIdentityCode :: BitWriter -> BitWriter
writeIdentityCode w =
  let w1 = writeBit False w  -- is_simple = 0 (normal code)

      -- Write code length code with only symbol 8 (for "length 8")
      -- Symbol 8 is at position 11 in kCodeLengthCodeOrder
      numCLC = 12  -- Include through position 11
      w2 = writeBits 4 (numCLC - 4) w1  -- Write 8, meaning 4+8=12 code lengths

      -- Write 12 lengths for kCodeLengthCodeOrder positions 0-11
      -- [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8]
      -- Only symbol 8 (position 11) gets length 1
      w3 = writeBits 3 0 w2   -- pos 0 (sym 17): len 0
      w4 = writeBits 3 0 w3   -- pos 1 (sym 18): len 0
      w5 = writeBits 3 0 w4   -- pos 2 (sym 0): len 0
      w6 = writeBits 3 0 w5   -- pos 3 (sym 1): len 0
      w7 = writeBits 3 0 w6   -- pos 4 (sym 2): len 0
      w8 = writeBits 3 0 w7   -- pos 5 (sym 3): len 0
      w9 = writeBits 3 0 w8   -- pos 6 (sym 4): len 0
      w10 = writeBits 3 0 w9  -- pos 7 (sym 5): len 0
      w11 = writeBits 3 0 w10 -- pos 8 (sym 16): len 0
      w12 = writeBits 3 0 w11 -- pos 9 (sym 6): len 0
      w13 = writeBits 3 0 w12 -- pos 10 (sym 7): len 0
      w14 = writeBits 3 1 w13 -- pos 11 (sym 8): len 1 ✓

      -- use_max_symbol = 0 (don't specify, use all 256)
      w15 = writeBit False w14

      -- Write 256 code lengths for symbols 0-255
      -- Each symbol gets length 8, encoded using symbol 8 from CLC
      -- Symbol 8 has code 0 (single symbol with len 1), so write bit 0 for each
      w16 = foldl (\wa _ -> writeBit False wa) w15 [0..255]

   in w16

-- | Write simple 1-symbol code
writeSimple1 :: Word16 -> BitWriter -> BitWriter
writeSimple1 sym w =
  writeBit True w |> writeBit False |> writeBit True |> writeBits 8 (fromIntegral sym)
  where (|>) = flip ($)

-- | Write all pixels with 8-bit identity codes
writeAllPixels :: VS.Vector Word8 -> Int -> BitWriter -> BitWriter
writeAllPixels pixels numPixels w =
  foldl (\wa i ->
    let g = pixels VS.! (i * 4 + 1)  -- Green
        r = pixels VS.! (i * 4)      -- Red
        b = pixels VS.! (i * 4 + 2)  -- Blue
        a = pixels VS.! (i * 4 + 3)  -- Alpha
     in wa |> writeBits 8 (fromIntegral g)
           |> writeBits 8 (fromIntegral r)
           |> writeBits 8 (fromIntegral b)
           |> writeBits 8 (fromIntegral a)
  ) w [0 .. numPixels - 1]
  where
    (|>) = flip ($)
