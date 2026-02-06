{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.Animation
  ( decodeAnimation,
    WebPAnimFrame (..),
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.Alpha
import Codec.Picture.WebP.Internal.Container
import Codec.Picture.WebP.Internal.VP8
import Codec.Picture.WebP.Internal.VP8L
import qualified Data.ByteString as B

-- | Animation frame with metadata
data WebPAnimFrame = WebPAnimFrame
  { webpFrameImage :: DynamicImage,
    webpFrameDuration :: Int,
    webpFrameX :: Int,
    webpFrameY :: Int
  }

-- | Decode animation frames from a WebP file
decodeAnimation :: WebPFile -> Either String [WebPAnimFrame]
decodeAnimation (WebPExtended _header chunks) = do
  _animHeader <- findAnimHeader chunks
  anmfChunks <- findAnimFrames chunks

  mapM decodeAnimFrame anmfChunks
decodeAnimation _ = Left "Not an animated WebP file"

-- | Find ANIM header
findAnimHeader :: [WebPChunk] -> Either String AnimHeader
findAnimHeader [] = Left "No ANIM chunk found"
findAnimHeader (ChunkANIM header : _) = Right header
findAnimHeader (_ : rest) = findAnimHeader rest

-- | Find all ANMF frames
findAnimFrames :: [WebPChunk] -> Either String [(AnimFrame, [WebPChunk])]
findAnimFrames chunks = Right $ collectFrames chunks
  where
    collectFrames [] = []
    collectFrames (ChunkANMF frame subChunks : rest) =
      (frame, subChunks) : collectFrames rest
    collectFrames (_ : rest) = collectFrames rest

-- | Decode a single animation frame
decodeAnimFrame :: (AnimFrame, [WebPChunk]) -> Either String WebPAnimFrame
decodeAnimFrame (frame, subChunks) = do
  let width = anmfWidth frame
      height = anmfHeight frame
      x = anmfX frame
      y = anmfY frame
      duration = anmfDuration frame

  maybeAlpha <- case findAlphaChunk subChunks of
    Right alphaData -> do
      alpha <- decodeAlpha width height alphaData
      return $ Just alpha
    Left _ -> return Nothing

  img <- case (findVP8InChunks subChunks, findVP8LInChunks subChunks) of
    (Right vp8Data, _) -> do
      baseImg <- decodeVP8 vp8Data
      return $ ImageRGB8 baseImg
    (_, Right vp8lData) -> do
      losslessImg <- decodeVP8L vp8lData
      return $ ImageRGBA8 losslessImg
    _ -> Left "No image data in animation frame"

  return $
    WebPAnimFrame
      { webpFrameImage = img,
        webpFrameDuration = duration,
        webpFrameX = x,
        webpFrameY = y
      }

-- Helper functions to find chunks

findAlphaChunk :: [WebPChunk] -> Either String B.ByteString
findAlphaChunk [] = Left "No ALPH chunk"
findAlphaChunk (ChunkALPH dat : _) = Right dat
findAlphaChunk (_ : rest) = findAlphaChunk rest

findVP8InChunks :: [WebPChunk] -> Either String B.ByteString
findVP8InChunks [] = Left "No VP8 chunk"
findVP8InChunks (ChunkVP8 dat : _) = Right dat
findVP8InChunks (_ : rest) = findVP8InChunks rest

findVP8LInChunks :: [WebPChunk] -> Either String B.ByteString
findVP8LInChunks [] = Left "No VP8L chunk"
findVP8LInChunks (ChunkVP8L dat : _) = Right dat
findVP8LInChunks (_ : rest) = findVP8LInChunks rest
