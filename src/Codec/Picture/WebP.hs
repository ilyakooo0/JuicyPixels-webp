{-# LANGUAGE OverloadedStrings #-}

module Codec.Picture.WebP
  ( -- * Decoding
    decodeWebP,
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

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.Alpha (decodeAlpha)
import Codec.Picture.WebP.Internal.Animation (WebPAnimFrame (..), combineRGBAlpha, decodeAnimFrame, decodeAnimation, decodeAnimationWithCompositing)
import Codec.Picture.WebP.Internal.AnimationEncode (AnimationFrame (..), encodeAnimation)
import Codec.Picture.WebP.Internal.Container
import Codec.Picture.WebP.Internal.Encode (encodeWebPLossless, encodeWebPLossy, encodeWebPLossyWithAlpha)
import Codec.Picture.WebP.Internal.VP8
import Codec.Picture.WebP.Internal.VP8L
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
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
  Int ->
  Int -> -- Canvas width, height
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
    WebPExtended _header chunks -> do
      -- Check for alpha channel
      case (findVP8Chunk chunks, findALPHChunk chunks) of
        (Right vp8Data, Right alphData) -> do
          -- VP8 + ALPH: decode both and combine
          -- The alpha plane has the VP8 frame's dimensions, which take
          -- precedence over the VP8X canvas size if they disagree
          rgbImg <- decodeVP8 vp8Data
          alphaVec <- decodeAlphaChunk alphData (imageWidth rgbImg) (imageHeight rgbImg)
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

-- | Decode first frame only (for animated images)
decodeWebPFirstFrame :: B.ByteString -> Either String DynamicImage
decodeWebPFirstFrame bs = do
  webpFile <- parseWebP bs
  case webpFile of
    WebPExtended header chunks
      | vp8xHasAnimation header,
        (anmf : _) <- [(frame, sub) | ChunkANMF frame sub <- chunks] ->
          webpFrameImage <$> decodeAnimFrame anmf
    _ -> decodeWebP bs

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
