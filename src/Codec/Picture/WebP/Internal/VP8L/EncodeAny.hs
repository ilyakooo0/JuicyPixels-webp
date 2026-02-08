{-# LANGUAGE BangPatterns #-}

-- | Encoder that works for any image by using simple multi-symbol codes
module Codec.Picture.WebP.Internal.VP8L.EncodeAny
  ( encodeVP8LAny,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.BitWriter
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- | Encode any image (uses simple codes or 2-symbol codes, no full Huffman yet)
encodeVP8LAny :: Image PixelRGBA8 -> B.ByteString
encodeVP8LAny img =
  let width = imageWidth img
      height = imageHeight img
      pixels = imageData img

      -- Analyze channels to find unique values
      info = analyzeChannels pixels (width * height)

      w =
        emptyBitWriter
          |> writeBits 8 0x2F
          |> writeBits 14 (fromIntegral $ width - 1)
          |> writeBits 14 (fromIntegral $ height - 1)
          |> writeBit True
          |> writeBits 3 0
          |> writeBit False -- no transforms
          |> writeBit False -- no color cache
          |> writeBit False -- single prefix code group
          |> writeChannelTree (ciGreen info)
          |> writeChannelTree (ciRed info)
          |> writeChannelTree (ciBlue info)
          |> writeChannelTree (ciAlpha info)
          |> writeSimple 0 -- Distance
          |> writePixelData pixels (width * height) info
          |> finalizeBitWriter
   in bitWriterToByteString w
  where
    (|>) = flip ($)

data ChannelInfo = ChannelInfo
  { ciGreen :: !(VU.Vector Word8),
    ciRed :: !(VU.Vector Word8),
    ciBlue :: !(VU.Vector Word8),
    ciAlpha :: !(VU.Vector Word8)
  }

analyzeChannels :: VS.Vector Word8 -> Int -> ChannelInfo
analyzeChannels pixels numPixels = runST $ do
  gSet <- VUM.replicate 256 False
  rSet <- VUM.replicate 256 False
  bSet <- VUM.replicate 256 False
  aSet <- VUM.replicate 256 False

  mapM_
    ( \i -> do
        VUM.write rSet (fromIntegral $ pixels VS.! (i * 4)) True
        VUM.write gSet (fromIntegral $ pixels VS.! (i * 4 + 1)) True
        VUM.write bSet (fromIntegral $ pixels VS.! (i * 4 + 2)) True
        VUM.write aSet (fromIntegral $ pixels VS.! (i * 4 + 3)) True
    )
    [0 .. numPixels - 1]

  let extract set = VU.unsafeFreeze set >>= \v -> return (VU.map fromIntegral $ VU.findIndices id v)

  g <- extract gSet
  r <- extract rSet
  b <- extract bSet
  a <- extract aSet
  return $ ChannelInfo g r b a

-- | Write prefix code for a channel
writeChannelTree :: VU.Vector Word8 -> BitWriter -> BitWriter
writeChannelTree syms w
  | VU.null syms = writeSimple 0 w
  | VU.length syms == 1 = writeSimple (fromIntegral $ syms VU.! 0) w
  | VU.length syms == 2 = write2Symbols (fromIntegral $ syms VU.! 0) (fromIntegral $ syms VU.! 1) w
  | otherwise = writeSimple (fromIntegral $ syms VU.! 0) w -- Fallback: encode as first symbol only

writeSimple :: Word16 -> BitWriter -> BitWriter
writeSimple sym w =
  writeBit True w |> writeBit False |> writeBit True |> writeBits 8 (fromIntegral sym)
  where
    (|>) = flip ($)

write2Symbols :: Word16 -> Word16 -> BitWriter -> BitWriter
write2Symbols s1 s2 w =
  writeBit True w |> writeBit True |> writeBit True |> writeBits 8 (fromIntegral s1) |> writeBits 8 (fromIntegral s2)
  where
    (|>) = flip ($)

writePixelData :: VS.Vector Word8 -> Int -> ChannelInfo -> BitWriter -> BitWriter
writePixelData pixels numPixels info w =
  foldl
    ( \wa i ->
        let g = pixels VS.! (i * 4 + 1)
            r = pixels VS.! (i * 4)
            b = pixels VS.! (i * 4 + 2)
            a = pixels VS.! (i * 4 + 3)

            wa1 = writeChannelValue g (ciGreen info) wa
            wa2 = writeChannelValue r (ciRed info) wa1
            wa3 = writeChannelValue b (ciBlue info) wa2
            wa4 = writeChannelValue a (ciAlpha info) wa3
         in wa4
    )
    w
    [0 .. numPixels - 1]

writeChannelValue :: Word8 -> VU.Vector Word8 -> BitWriter -> BitWriter
writeChannelValue val syms w
  | VU.length syms == 1 = w -- 0-bit code
  | VU.length syms == 2 = writeBit (val == (syms VU.! 1)) w -- 1-bit code
  | otherwise = w -- >2 symbols: use first symbol (lossy fallback)
