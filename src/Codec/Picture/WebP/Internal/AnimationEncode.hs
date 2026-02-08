{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.AnimationEncode
  ( encodeAnimation,
    AnimationFrame (..),
    makeANIMChunk,
    makeANMFChunk,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.AlphaEncode
import Codec.Picture.WebP.Internal.Encode
import qualified Codec.Picture.WebP.Internal.VP8.Encode as VP8.Encode
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word

-- | Frame for animation encoding
data AnimationFrame = AnimationFrame
  { frameImage :: DynamicImage,
    frameDuration :: Int, -- milliseconds
    frameX :: Int,
    frameY :: Int,
    frameBlend :: Bool, -- True = alpha blend, False = no blend
    frameDispose :: Bool -- True = dispose to background, False = don't dispose
  }

-- | Encode animation frames to WebP
-- Returns complete WebP data with ANIM and ANMF chunks
encodeAnimation ::
  [AnimationFrame] -> -- Frames to encode
  Int -> Int -> -- Canvas width, height
  Word32 -> -- Background color (BGRA)
  Word16 -> -- Loop count (0 = infinite)
  Int -> -- Quality (0-100)
  B.ByteString
encodeAnimation frames canvasWidth canvasHeight bgColor loopCount quality =
  let -- Create VP8X header
      vp8xChunk = makeVP8XChunk canvasWidth canvasHeight False True -- hasAlpha=False, hasAnim=True

      -- Create ANIM chunk
      animChunk = makeANIMChunk bgColor loopCount

      -- Create ANMF chunks for each frame
      anmfChunks = map (encodeAnimFrame quality) frames

      -- Combine all chunks
      allChunks = vp8xChunk <> animChunk <> mconcat anmfChunks
      totalSize = B.length allChunks

   in makeRIFFContainer (fromIntegral totalSize) allChunks

-- | Encode a single animation frame
encodeAnimFrame :: Int -> AnimationFrame -> B.ByteString
encodeAnimFrame quality frame =
  let -- Encode frame image
      imageChunk = case frameImage frame of
        ImageRGB8 img -> makeVP8Chunk (VP8.Encode.encodeVP8 img quality)
        ImageRGBA8 img ->
          -- For RGBA, need ALPH + VP8
          let rgbImg = pixelMap (\(PixelRGBA8 r g b _) -> PixelRGB8 r g b) img
              vp8Data = VP8.Encode.encodeVP8 rgbImg quality
              vp8Chunk = makeVP8Chunk vp8Data
              alphaData = encodeAlpha img
              alphChunk = makeALPHChunk alphaData
           in alphChunk <> vp8Chunk
        _ -> error "Unsupported image format for animation encoding"

      -- Create ANMF chunk containing the frame
   in makeANMFChunk frame imageChunk

-- | Create ANIM chunk
makeANIMChunk :: Word32 -> Word16 -> B.ByteString
makeANIMChunk bgColor loopCount =
  let fourCC = B.pack [65, 78, 73, 77] -- "ANIM"
      chunkSize = BL.toStrict $ runPut $ putWord32le 6 -- ANIM payload is 6 bytes

      payload = BL.toStrict $ runPut $ do
        putWord32le bgColor -- Background color (BGRA)
        putWord16le loopCount -- Loop count

      padding = B.empty -- 6 bytes is even, no padding needed

   in fourCC <> chunkSize <> payload <> padding

-- | Create ANMF chunk for a single frame
makeANMFChunk :: AnimationFrame -> B.ByteString -> B.ByteString
makeANMFChunk frame imageData =
  let fourCC = B.pack [65, 78, 77, 70] -- "ANMF"

      -- Frame coordinates (2x the actual value, 24 bits each)
      x24 = fromIntegral (frameX frame * 2) :: Word32
      y24 = fromIntegral (frameY frame * 2) :: Word32

      -- Frame dimensions come from the image data
      -- For now, assume full canvas
      width24 = 0 :: Word32 -- Will be set from image
      height24 = 0 :: Word32

      -- Duration in milliseconds (24 bits)
      duration24 = fromIntegral (frameDuration frame) :: Word32

      -- Flags byte:
      -- Bit 0: reserved
      -- Bit 1: dispose (0 = none, 1 = dispose to background)
      -- Bit 2: blend (0 = no blend/replace, 1 = alpha blend)
      -- Bits 3-7: reserved
      flags =
        ( if frameDispose frame then bit 1 else 0 )
        .|. ( if frameBlend frame then bit 2 else 0 )

      header = BL.toStrict $ runPut $ do
        -- X coordinate (24 bits)
        putWord8 (fromIntegral $ x24 .&. 0xFF)
        putWord8 (fromIntegral $ (x24 `shiftR` 8) .&. 0xFF)
        putWord8 (fromIntegral $ (x24 `shiftR` 16) .&. 0xFF)

        -- Y coordinate (24 bits)
        putWord8 (fromIntegral $ y24 .&. 0xFF)
        putWord8 (fromIntegral $ (y24 `shiftR` 8) .&. 0xFF)
        putWord8 (fromIntegral $ (y24 `shiftR` 16) .&. 0xFF)

        -- Width - 1 (24 bits)
        putWord8 (fromIntegral $ width24 .&. 0xFF)
        putWord8 (fromIntegral $ (width24 `shiftR` 8) .&. 0xFF)
        putWord8 (fromIntegral $ (width24 `shiftR` 16) .&. 0xFF)

        -- Height - 1 (24 bits)
        putWord8 (fromIntegral $ height24 .&. 0xFF)
        putWord8 (fromIntegral $ (height24 `shiftR` 8) .&. 0xFF)
        putWord8 (fromIntegral $ (height24 `shiftR` 16) .&. 0xFF)

        -- Duration (24 bits)
        putWord8 (fromIntegral $ duration24 .&. 0xFF)
        putWord8 (fromIntegral $ (duration24 `shiftR` 8) .&. 0xFF)
        putWord8 (fromIntegral $ (duration24 `shiftR` 16) .&. 0xFF)

        -- Flags
        putWord8 flags

      payload = header <> imageData
      chunkSize = BL.toStrict $ runPut $ putWord32le (fromIntegral $ B.length payload)
      padding = if odd (B.length payload) then B.singleton 0 else B.empty

   in fourCC <> chunkSize <> payload <> padding
