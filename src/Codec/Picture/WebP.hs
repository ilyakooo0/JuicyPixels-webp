{-# LANGUAGE OverloadedStrings #-}

module Codec.Picture.WebP
  ( -- * Decoding
    decodeWebP,
    decodeWebPWithMetadata,
    decodeWebPFirstFrame,
    decodeWebPAnimation,
    decodeWebPAnimationComposited,
    WebPAnimFrame (..),

    -- * Encoding
    encodeWebPLossless,
    encodeWebPLossy,
    encodeWebPLossyWithAlpha,
    encodeWebPAnimation,
    WebPEncodeFrame (..),
  )
where

import Codec.Picture.Metadata
import Codec.Picture.Types
import Codec.Picture.WebP.Internal.Alpha (decodeAlpha)
import Codec.Picture.WebP.Internal.Animation (WebPAnimFrame (..), decodeAnimation, decodeAnimationWithCompositing)
import Codec.Picture.WebP.Internal.AnimationEncode (AnimationFrame (..), encodeAnimation)
import Codec.Picture.WebP.Internal.Container
import Codec.Picture.WebP.Internal.Encode (encodeWebPLossless, encodeWebPLossy, encodeWebPLossyWithAlpha)
import Codec.Picture.WebP.Internal.VP8
import Codec.Picture.WebP.Internal.VP8L
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Control.Monad.ST
import Control.Monad (forM_)
import Data.Word

-- | Frame for encoding animations
data WebPEncodeFrame = WebPEncodeFrame
  { webpEncodeFrameImage :: DynamicImage,
    webpEncodeFrameDuration :: Int, -- milliseconds
    webpEncodeFrameX :: Int,
    webpEncodeFrameY :: Int
  }

-- | Encode animation to WebP
encodeWebPAnimation ::
  [WebPEncodeFrame] -> -- Frames to encode
  Int -> Int -> -- Canvas width, height
  Int -> -- Quality (0-100)
  B.ByteString
encodeWebPAnimation frames canvasWidth canvasHeight quality =
  let animFrames = map toAnimFrame frames
      bgColor = 0xFFFFFFFF :: Word32 -- White background (BGRA)
      loopCount = 0 :: Word16 -- Infinite loop
   in encodeAnimation animFrames canvasWidth canvasHeight bgColor loopCount quality
  where
    toAnimFrame f =
      AnimationFrame
        { frameImage = webpEncodeFrameImage f,
          frameDuration = webpEncodeFrameDuration f,
          frameX = webpEncodeFrameX f,
          frameY = webpEncodeFrameY f,
          frameBlend = True,
          frameDispose = False
        }

-- | Decode a WebP image
-- Supports both VP8L (lossless) and VP8 (lossy) formats
decodeWebP :: B.ByteString -> Either String DynamicImage
decodeWebP bs = do
  webpFile <- parseWebP bs
  case webpFile of
    WebPSimpleLossless vp8lData -> do
      img <- decodeVP8L vp8lData
      return $ ImageRGBA8 img
    WebPSimpleLossy vp8Data -> do
      img <- decodeVP8 vp8Data
      return $ ImageRGB8 img
    WebPExtended header chunks -> do
      -- Check for alpha channel
      case (findVP8Chunk chunks, findALPHChunk chunks) of
        (Right vp8Data, Right alphData) -> do
          -- VP8 + ALPH: decode both and combine
          rgbImg <- decodeVP8 vp8Data
          alphaVec <- decodeAlphaChunk alphData (vp8xCanvasWidth header) (vp8xCanvasHeight header)
          let rgbaImg = combineRGBAlpha rgbImg alphaVec
          return $ ImageRGBA8 rgbaImg
        (Right vp8Data, Left _) -> do
          -- VP8 without alpha
          img <- decodeVP8 vp8Data
          return $ ImageRGB8 img
        (Left _, Right _) -> Left "VP8X has ALPH but no VP8 chunk"
        (Left _, Left _) -> do
          -- No VP8, try VP8L
          vp8lChunk <- findVP8LChunk chunks
          img <- decodeVP8L vp8lChunk
          return $ ImageRGBA8 img

-- | Decode WebP with metadata
decodeWebPWithMetadata :: B.ByteString -> Either String (DynamicImage, Metadatas)
decodeWebPWithMetadata bs = do
  webpFile <- parseWebP bs
  case webpFile of
    WebPSimpleLossless vp8lData -> do
      img <- decodeVP8L vp8lData
      let meta = extractMetadata webpFile
      return (ImageRGBA8 img, meta)
    WebPSimpleLossy vp8Data -> do
      img <- decodeVP8 vp8Data
      let meta = extractMetadata webpFile
      return (ImageRGB8 img, meta)
    WebPExtended header chunks -> do
      case findVP8Chunk chunks of
        Right vp8Data -> do
          img <- decodeVP8 vp8Data
          let meta = extractMetadataExtended header chunks
          return (ImageRGB8 img, meta)
        Left _ -> do
          vp8lChunk <- findVP8LChunk chunks
          img <- decodeVP8L vp8lChunk
          let meta = extractMetadataExtended header chunks
          return (ImageRGBA8 img, meta)

-- | Decode first frame only (for animated images)
decodeWebPFirstFrame :: B.ByteString -> Either String DynamicImage
decodeWebPFirstFrame = decodeWebP

-- | Decode animation frames
decodeWebPAnimation :: B.ByteString -> Either String [WebPAnimFrame]
decodeWebPAnimation bs = do
  webpFile <- parseWebP bs
  decodeAnimation webpFile

-- Helper functions

findVP8Chunk :: [WebPChunk] -> Either String B.ByteString
findVP8Chunk [] = Left "No VP8 chunk found"
findVP8Chunk (ChunkVP8 dat : _) = Right dat
findVP8Chunk (_ : rest) = findVP8Chunk rest

findVP8LChunk :: [WebPChunk] -> Either String B.ByteString
findVP8LChunk [] = Left "No VP8L chunk found"
findVP8LChunk (ChunkVP8L dat : _) = Right dat
findVP8LChunk (_ : rest) = findVP8LChunk rest

findALPHChunk :: [WebPChunk] -> Either String B.ByteString
findALPHChunk [] = Left "No ALPH chunk found"
findALPHChunk (ChunkALPH dat : _) = Right dat
findALPHChunk (_ : rest) = findALPHChunk rest

-- | Decode ALPH chunk
decodeAlphaChunk :: B.ByteString -> Int -> Int -> Either String (VS.Vector Word8)
decodeAlphaChunk alphData width height = decodeAlpha width height alphData

-- | Combine RGB8 image with alpha channel to create RGBA8
combineRGBAlpha :: Image PixelRGB8 -> VS.Vector Word8 -> Image PixelRGBA8
combineRGBAlpha rgbImg alphaVec = runST $ do
  let width = imageWidth rgbImg
      height = imageHeight rgbImg
      rgbData = imageData rgbImg

  pixels <- VSM.new (width * height * 4)

  forM_ [0 .. height - 1] $ \y ->
    forM_ [0 .. width - 1] $ \x -> do
      let rgbIdx = (y * width + x) * 3
          alphaIdx = y * width + x
          pixelIdx = (y * width + x) * 4

      let r = rgbData VS.! rgbIdx
          g = rgbData VS.! (rgbIdx + 1)
          b = rgbData VS.! (rgbIdx + 2)
          a = alphaVec VS.! alphaIdx

      VSM.write pixels pixelIdx r
      VSM.write pixels (pixelIdx + 1) g
      VSM.write pixels (pixelIdx + 2) b
      VSM.write pixels (pixelIdx + 3) a

  finalPixels <- VS.unsafeFreeze pixels
  return $ Image width height finalPixels

extractMetadata :: WebPFile -> Metadatas
extractMetadata _ = mempty

extractMetadataExtended :: VP8XHeader -> [WebPChunk] -> Metadatas
extractMetadataExtended _header chunks =
  extractChunkMetadata chunks

extractChunkMetadata :: [WebPChunk] -> Metadatas
extractChunkMetadata [] = mempty
extractChunkMetadata (ChunkEXIF _dat : rest) =
  extractChunkMetadata rest
extractChunkMetadata (_ : rest) = extractChunkMetadata rest

-- | Decode animation frames with proper canvas compositing
-- Returns fully composited RGBA8 frames ready for display
decodeWebPAnimationComposited :: B.ByteString -> Either String [Image PixelRGBA8]
decodeWebPAnimationComposited bs = do
  webpFile <- parseWebP bs
  case webpFile of
    WebPExtended header _ -> do
      let width = vp8xCanvasWidth header
          height = vp8xCanvasHeight header
      decodeAnimationWithCompositing width height webpFile
    _ -> Left "Not an animated WebP file (no VP8X header)"
