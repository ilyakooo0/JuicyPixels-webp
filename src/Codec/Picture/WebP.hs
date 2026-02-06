{-# LANGUAGE OverloadedStrings #-}

module Codec.Picture.WebP
  ( decodeWebP,
    decodeWebPWithMetadata,
    decodeWebPFirstFrame,
    decodeWebPAnimation,
    WebPAnimFrame (..),
  )
where

import Codec.Picture.Metadata
import Codec.Picture.Types
import Codec.Picture.WebP.Internal.Animation (WebPAnimFrame (..), decodeAnimation)
import Codec.Picture.WebP.Internal.Container
import Codec.Picture.WebP.Internal.VP8
import Codec.Picture.WebP.Internal.VP8L
import qualified Data.ByteString as B

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
      case findVP8Chunk chunks of
        Right vp8Data -> do
          img <- decodeVP8 vp8Data
          return $ ImageRGB8 img
        Left _ -> do
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
