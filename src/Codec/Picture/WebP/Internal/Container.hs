{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Picture.WebP.Internal.Container
  ( WebPFile (..),
    VP8XHeader (..),
    WebPChunk (..),
    AnimHeader (..),
    AnimFrame (..),
    FourCC,
    parseWebP,
  )
where

import Control.Monad (replicateM, unless, when)
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word

type FourCC = B.ByteString

-- | Top-level WebP file structure
data WebPFile
  = WebPSimpleLossy !B.ByteString
  | WebPSimpleLossless !B.ByteString
  | WebPExtended !VP8XHeader ![WebPChunk]
  deriving (Show, Eq)

-- | VP8X extended format header
data VP8XHeader = VP8XHeader
  { vp8xHasICC :: !Bool,
    vp8xHasAlpha :: !Bool,
    vp8xHasExif :: !Bool,
    vp8xHasXMP :: !Bool,
    vp8xHasAnimation :: !Bool,
    vp8xCanvasWidth :: !Int,
    vp8xCanvasHeight :: !Int
  }
  deriving (Show, Eq)

-- | Animation header
data AnimHeader = AnimHeader
  { animBackgroundColor :: !Word32,
    animLoopCount :: !Word16
  }
  deriving (Show, Eq)

-- | Animation frame
data AnimFrame = AnimFrame
  { anmfX :: !Int,
    anmfY :: !Int,
    anmfWidth :: !Int,
    anmfHeight :: !Int,
    anmfDuration :: !Int,
    anmfBlend :: !Bool,
    anmfDispose :: !Bool
  }
  deriving (Show, Eq)

-- | Individual WebP chunks
data WebPChunk
  = ChunkVP8 !B.ByteString
  | ChunkVP8L !B.ByteString
  | ChunkALPH !B.ByteString
  | ChunkANIM !AnimHeader
  | ChunkANMF !AnimFrame ![WebPChunk]
  | ChunkICCP !B.ByteString
  | ChunkEXIF !B.ByteString
  | ChunkXMP !B.ByteString
  | ChunkUnknown !FourCC !B.ByteString
  deriving (Show, Eq)

-- | Parse a WebP file from ByteString
parseWebP :: B.ByteString -> Either String WebPFile
parseWebP bs =
  case runGetOrFail getWebP (BL.fromStrict bs) of
    Left (_, _, err) -> Left err
    Right (_, _, result) -> Right result

-- | Main WebP parser
getWebP :: Get WebPFile
getWebP = do
  riffTag <- getByteString 4
  unless (riffTag == "RIFF") $
    fail "Not a RIFF file"

  fileSize <- getWord32le
  _ <- return fileSize

  webpTag <- getByteString 4
  unless (webpTag == "WEBP") $
    fail "Not a WebP file"

  firstChunkTag <- lookAhead (getByteString 4)

  case firstChunkTag of
    "VP8 " -> WebPSimpleLossy <$> getChunkData "VP8 "
    "VP8L" -> WebPSimpleLossless <$> getChunkData "VP8L"
    "VP8X" -> do
      header <- getVP8XChunk
      chunks <- getChunksUntilEnd
      return $ WebPExtended header chunks
    _ -> fail $ "Unknown first chunk: " ++ show firstChunkTag

-- | Get VP8X header
getVP8XChunk :: Get VP8XHeader
getVP8XChunk = do
  tag <- getByteString 4
  unless (tag == "VP8X") $
    fail "Expected VP8X chunk"

  chunkSize <- getWord32le
  unless (chunkSize == 10) $
    fail "VP8X chunk size must be 10"

  flags <- getWord8
  let hasICC = testBit flags 5
      hasAlpha = testBit flags 4
      hasExif = testBit flags 3
      hasXMP = testBit flags 2
      hasAnimation = testBit flags 1

  _reserved <- getWord8
  _reserved2 <- getWord8
  _reserved3 <- getWord8

  canvasWidthMinus1 <- getWord24le
  canvasHeightMinus1 <- getWord24le

  return $
    VP8XHeader
      { vp8xHasICC = hasICC,
        vp8xHasAlpha = hasAlpha,
        vp8xHasExif = hasExif,
        vp8xHasXMP = hasXMP,
        vp8xHasAnimation = hasAnimation,
        vp8xCanvasWidth = fromIntegral canvasWidthMinus1 + 1,
        vp8xCanvasHeight = fromIntegral canvasHeightMinus1 + 1
      }

-- | Read chunks until end of input
getChunksUntilEnd :: Get [WebPChunk]
getChunksUntilEnd = do
  empty <- isEmpty
  if empty
    then return []
    else do
      chunk <- getChunk
      rest <- getChunksUntilEnd
      return (chunk : rest)

-- | Parse a single chunk
getChunk :: Get WebPChunk
getChunk = do
  fourCC <- getByteString 4
  chunkSize <- getWord32le
  let size = fromIntegral chunkSize

  case fourCC of
    "VP8 " -> ChunkVP8 <$> getByteString size <* skipPadding chunkSize
    "VP8L" -> ChunkVP8L <$> getByteString size <* skipPadding chunkSize
    "ALPH" -> ChunkALPH <$> getByteString size <* skipPadding chunkSize
    "ANIM" -> do
      chunk <- getANIMChunk
      skipPadding chunkSize
      return $ ChunkANIM chunk
    "ANMF" -> do
      chunk <- getANMFChunk size
      skipPadding chunkSize
      return chunk
    "ICCP" -> ChunkICCP <$> getByteString size <* skipPadding chunkSize
    "EXIF" -> ChunkEXIF <$> getByteString size <* skipPadding chunkSize
    "XMP " -> ChunkXMP <$> getByteString size <* skipPadding chunkSize
    _ -> ChunkUnknown fourCC <$> getByteString size <* skipPadding chunkSize

-- | Get chunk data for simple formats
getChunkData :: FourCC -> Get B.ByteString
getChunkData expectedTag = do
  tag <- getByteString 4
  unless (tag == expectedTag) $
    fail $
      "Expected " ++ show expectedTag ++ " chunk"

  chunkSize <- getWord32le
  let size = fromIntegral chunkSize

  payload <- getByteString size
  skipPadding chunkSize
  return payload

-- | Parse ANIM chunk
getANIMChunk :: Get AnimHeader
getANIMChunk = do
  bgColor <- getWord32le
  loopCount <- getWord16le
  return $ AnimHeader bgColor loopCount

-- | Parse ANMF chunk
getANMFChunk :: Int -> Get WebPChunk
getANMFChunk totalSize = do
  frameX <- (* 2) . fromIntegral <$> getWord24le
  frameY <- (* 2) . fromIntegral <$> getWord24le
  frameWidthMinus1 <- getWord24le
  frameHeightMinus1 <- getWord24le
  duration <- fromIntegral <$> getWord24le

  flags <- getWord8
  let blend = testBit flags 1
      dispose = testBit flags 0

  let headerSize = 16
      remainingSize = totalSize - headerSize

  subChunksData <- getByteString remainingSize

  let frame =
        AnimFrame
          { anmfX = frameX,
            anmfY = frameY,
            anmfWidth = fromIntegral frameWidthMinus1 + 1,
            anmfHeight = fromIntegral frameHeightMinus1 + 1,
            anmfDuration = duration,
            anmfBlend = blend,
            anmfDispose = dispose
          }

  case runGetOrFail getChunksUntilEnd (BL.fromStrict subChunksData) of
    Left (_, _, err) -> fail $ "Failed to parse ANMF sub-chunks: " ++ err
    Right (_, _, subChunks) -> return $ ChunkANMF frame subChunks

-- | Skip padding byte if chunk size is odd
skipPadding :: Word32 -> Get ()
skipPadding chunkSize = when (chunkSize .&. 1 == 1) $ skip 1

-- | Read 24-bit little-endian value
getWord24le :: Get Word32
getWord24le = do
  b0 <- fromIntegral <$> getWord8
  b1 <- fromIntegral <$> getWord8
  b2 <- fromIntegral <$> getWord8
  return $ b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16)
