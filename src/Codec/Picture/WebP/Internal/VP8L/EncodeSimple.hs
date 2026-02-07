{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8L.EncodeSimple
  ( encodeVP8LSimple,
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

-- | Simple VP8L encoder
encodeVP8LSimple :: Image PixelRGBA8 -> B.ByteString
encodeVP8LSimple img =
  let width = imageWidth img
      height = imageHeight img

      -- Convert to ARGB WITHOUT transform for now (to test)
      argbPixels = convertWithoutTransform img

      -- Analyze channels
      info = analyzeChannels argbPixels

      -- Build bitstream
      w = emptyBitWriter
        |> writeBits 8 0x2F  -- Signature
        |> writeBits 14 (fromIntegral $ width - 1)
        |> writeBits 14 (fromIntegral $ height - 1)
        |> writeBit True  -- alpha_is_used
        |> writeBits 3 0  -- version
        |> writeBit False  -- NO transform (testing)
        |> writeBit False  -- no color cache
        |> writeBit False  -- single prefix code group
        |> writeChannelCode (ciGreen info)
        |> writeChannelCode (ciRed info)
        |> writeChannelCode (ciBlue info)
        |> writeChannelCode (ciAlpha info)
        |> writeSimple1 0  -- Distance code
        |> writeOnePixels argbPixels info
        |> finalizeBitWriter

   in bitWriterToByteString w
  where
    (|>) = flip ($)

convertWithoutTransform :: Image PixelRGBA8 -> VS.Vector Word32
convertWithoutTransform img = VS.generate (imageWidth img * imageHeight img) $ \i ->
  let pixels = imageData img
      r = pixels VS.! (i * 4)
      g = pixels VS.! (i * 4 + 1)
      b = pixels VS.! (i * 4 + 2)
      a = pixels VS.! (i * 4 + 3)
   in packARGB a r g b

data ChannelInfo = ChannelInfo
  { ciGreen :: !(VU.Vector Word8),
    ciRed :: !(VU.Vector Word8),
    ciBlue :: !(VU.Vector Word8),
    ciAlpha :: !(VU.Vector Word8)
  }

convertAndTransform :: Image PixelRGBA8 -> VS.Vector Word32
convertAndTransform img = VS.generate (imageWidth img * imageHeight img) $ \i ->
  let pixels = imageData img
      r = pixels VS.! (i * 4)
      g = pixels VS.! (i * 4 + 1)
      b = pixels VS.! (i * 4 + 2)
      a = pixels VS.! (i * 4 + 3)
      r' = (r - g) .&. 0xFF
      b' = (b - g) .&. 0xFF
   in packARGB a r' g b'

analyzeChannels :: VS.Vector Word32 -> ChannelInfo
analyzeChannels pixels = runST $ do
  gSet <- VUM.replicate 256 False
  rSet <- VUM.replicate 256 False
  bSet <- VUM.replicate 256 False
  aSet <- VUM.replicate 256 False

  VS.forM_ pixels $ \px -> do
    VUM.write gSet (fromIntegral $ (px `shiftR` 8) .&. 0xFF) True
    VUM.write rSet (fromIntegral $ (px `shiftR` 16) .&. 0xFF) True
    VUM.write bSet (fromIntegral $ px .&. 0xFF) True
    VUM.write aSet (fromIntegral $ (px `shiftR` 24) .&. 0xFF) True

  let extract set = VU.unsafeFreeze set >>= \v -> return (VU.map fromIntegral $ VU.findIndices id v)

  g <- extract gSet
  r <- extract rSet
  b <- extract bSet
  a <- extract aSet
  return $ ChannelInfo g r b a

writeChannelCode :: VU.Vector Word8 -> BitWriter -> BitWriter
writeChannelCode syms w
  | VU.null syms = writeSimple1 0 w
  | VU.length syms == 1 = writeSimple1 (fromIntegral $ syms VU.! 0) w
  | VU.length syms == 2 = writeSimple2 (fromIntegral $ syms VU.! 0) (fromIntegral $ syms VU.! 1) w
  | VU.length syms <= 256 = writeFixedLengthCode syms w
  | otherwise = writeSimple1 0 w  -- Shouldn't happen

writeSimple1 :: Word16 -> BitWriter -> BitWriter
writeSimple1 sym w =
  writeBit True w |> writeBit False |> writeBit True |> writeBits 8 (fromIntegral sym)
  where (|>) = flip ($)

writeSimple2 :: Word16 -> Word16 -> BitWriter -> BitWriter
writeSimple2 s1 s2 w =
  writeBit True w |> writeBit True |> writeBit True |> writeBits 8 (fromIntegral s1) |> writeBits 8 (fromIntegral s2)
  where (|>) = flip ($)

-- | Write fixed-length code (for 3-256 symbols)
writeFixedLengthCode :: VU.Vector Word8 -> BitWriter -> BitWriter
writeFixedLengthCode syms w =
  let numSyms = VU.length syms
      codeLen = ceilLog2 numSyms

      -- Use simple strategy: code length code with just symbol 8 and symbol 18
      -- Symbol 8 (literal 8) and symbol 18 (repeat 0)
      w1 = writeBit False w  -- is_simple = 0

      -- Code length code: symbols 8 and 18
      -- kCodeLengthCodeOrder = [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, ...]
      -- Position 1: symbol 18, Position 11: symbol 8
      numCLC = 12
      w2 = writeBits 4 (numCLC - 4) w1

      -- Write CLC lengths: symbol 18 gets len 1, symbol 8 gets len 1, rest len 0
      w3 = writeBits 3 0 w2   -- pos 0 (sym 17): len 0
      w4 = writeBits 3 1 w3   -- pos 1 (sym 18): len 1
      w5 = writeBits 3 0 w4   -- pos 2 (sym 0): len 0
      w6 = writeBits 3 0 w5   -- pos 3-10: len 0
      w7 = writeBits 3 0 w6
      w8 = writeBits 3 0 w7
      w9 = writeBits 3 0 w8
      w10 = writeBits 3 0 w9
      w11 = writeBits 3 0 w10
      w12 = writeBits 3 0 w11
      w13 = writeBits 3 0 w12
      w14 = writeBits 3 1 w13 -- pos 11 (sym 8): len 1

      -- use_max_symbol = 1
      w15 = writeBit True w14
      maxSymBits = max 2 (ceilLog2 255)  -- For 256 symbols
      w16 = writeBits 3 (fromIntegral $ (maxSymBits - 2) `div` 2) w15
      w17 = writeBits maxSymBits 254 w16  -- max_symbol encoding for 256

      -- Write code lengths: all 256 symbols get length 8
      -- Using CLC with symbols 8 (code 0) and 18 (code 1):
      -- Canonical: 8→0, 18→1
      -- Write symbol 8 first (literal 8)
      w18 = writeBit False w17  -- Symbol 8 (code 0)

      -- Then use symbol 18 to repeat 0 for remaining positions? No, we need to repeat 8!
      -- Symbol 18 repeats 0, not previous. We need symbol 16 for repeat previous.
      -- But symbol 16 needs len 1 in CLC. Let me add it.

      -- Actually, simplest working solution: write symbol 8 (lit 8) for each of 256 symbols
      -- Symbol 8 has code 0 (len 1), so write bit 0, 256 times
      w19 = foldl (\wa _ -> writeBit False wa) w17 [1..256]

   in w19

writeOnePixels :: VS.Vector Word32 -> ChannelInfo -> BitWriter -> BitWriter
writeOnePixels pixels info writer =
  VS.foldl' (writeOnePixel info) writer pixels

writeOnePixel :: ChannelInfo -> BitWriter -> Word32 -> BitWriter
writeOnePixel info w px =
  let g = fromIntegral ((px `shiftR` 8) .&. 0xFF)
      r = fromIntegral ((px `shiftR` 16) .&. 0xFF)
      b = fromIntegral (px .&. 0xFF)
      a = fromIntegral ((px `shiftR` 24) .&. 0xFF)
      -- Note: VP8L pixel encoding order is Green, Red, Blue, Alpha
   in w |> writeValue g (ciGreen info)
         |> writeValue r (ciRed info)
         |> writeValue b (ciBlue info)
         |> writeValue a (ciAlpha info)
  where (|>) = flip ($)

writeValue :: Word8 -> VU.Vector Word8 -> BitWriter -> BitWriter
writeValue val syms w
  | VU.length syms == 1 = w  -- 0-bit code
  | VU.length syms == 2 = writeBit (val == (syms VU.! 1)) w  -- 1-bit code
  | otherwise =
      -- Fixed-length code: find symbol index and write it
      case VU.findIndex (== val) syms of
        Just idx ->
          let codeLen = ceilLog2 (VU.length syms)
           in writeBits codeLen (fromIntegral idx) w
        Nothing -> w  -- Symbol not found (shouldn't happen)

packARGB :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
packARGB a r g b =
  (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

ceilLog2 :: Int -> Int
ceilLog2 n
  | n <= 1 = 0
  | n <= 2 = 1
  | n <= 4 = 2
  | n <= 8 = 3
  | n <= 16 = 4
  | n <= 32 = 5
  | n <= 64 = 6
  | n <= 128 = 7
  | otherwise = 8
