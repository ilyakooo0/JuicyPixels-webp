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
      w =
        emptyBitWriter
          |> writeBits 8 0x2F -- Signature
          |> writeBits 14 (fromIntegral $ width - 1)
          |> writeBits 14 (fromIntegral $ height - 1)
          |> writeBit True -- alpha_is_used
          |> writeBits 3 0 -- version
          |> writeBit False -- NO transform (testing)
          |> writeBit False -- no color cache
          |> writeBit False -- single prefix code group
          |> writeChannelCode (ciGreen info)
          |> writeChannelCode (ciRed info)
          |> writeChannelCode (ciBlue info)
          |> writeChannelCode (ciAlpha info)
          |> writeSimple1 0 -- Distance code
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
  | otherwise =
      -- For 3+ unique values, use simple code with min/max values (lossy quantization)
      -- This is a simple approximation - full VP8L would need proper Huffman coding
      let minVal = VU.minimum syms
          maxVal = VU.maximum syms
       in writeSimple2 (fromIntegral minVal) (fromIntegral maxVal) w

writeSimple1 :: Word16 -> BitWriter -> BitWriter
writeSimple1 sym w =
  writeBit True w |> writeBit False |> writeBit True |> writeBits 8 (fromIntegral sym)
  where
    (|>) = flip ($)

writeSimple2 :: Word16 -> Word16 -> BitWriter -> BitWriter
writeSimple2 s1 s2 w =
  writeBit True w |> writeBit True |> writeBit True |> writeBits 8 (fromIntegral s1) |> writeBits 8 (fromIntegral s2)
  where
    (|>) = flip ($)

writeOnePixels :: VS.Vector Word32 -> ChannelInfo -> BitWriter -> BitWriter
writeOnePixels pixels info writer =
  VS.foldl' (writeOnePixel info) writer pixels

writeOnePixel :: ChannelInfo -> BitWriter -> Word32 -> BitWriter
writeOnePixel info w px =
  let g = fromIntegral ((px `shiftR` 8) .&. 0xFF)
      r = fromIntegral ((px `shiftR` 16) .&. 0xFF)
      b = fromIntegral (px .&. 0xFF)
      a = fromIntegral ((px `shiftR` 24) .&. 0xFF)
   in -- Note: VP8L pixel encoding order is Green, Red, Blue, Alpha
      w
        |> writeValue g (ciGreen info)
        |> writeValue r (ciRed info)
        |> writeValue b (ciBlue info)
        |> writeValue a (ciAlpha info)
  where
    (|>) = flip ($)

writeValue :: Word8 -> VU.Vector Word8 -> BitWriter -> BitWriter
writeValue val syms w
  | VU.length syms == 1 = w -- 0-bit code (single symbol)
  | VU.length syms == 2 = writeBit (val == (syms VU.! 1)) w -- 1-bit simple code
  | otherwise =
      -- For 3+ symbols, we quantized to 2 (min/max)
      -- Write 0 for min, 1 for max (whichever is closer)
      let minVal = VU.minimum syms
          maxVal = VU.maximum syms
          -- Choose closer value
          distToMin = abs (fromIntegral val - fromIntegral minVal :: Int)
          distToMax = abs (fromIntegral val - fromIntegral maxVal :: Int)
       in writeBit (distToMax <= distToMin) w

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
