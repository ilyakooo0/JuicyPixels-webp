{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.Animation
  ( decodeAnimation,
    decodeAnimationWithCompositing,
    decodeAnimFrame,
    combineRGBAlpha,
    WebPAnimFrame (..),
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.Alpha
import Codec.Picture.WebP.Internal.Container
import Codec.Picture.WebP.Internal.VP8
import Codec.Picture.WebP.Internal.VP8L
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import Data.Maybe (catMaybes)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word

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
  let x = anmfX frame
      y = anmfY frame
      duration = anmfDuration frame

  img <- case (findVP8InChunks subChunks, findVP8LInChunks subChunks) of
    (Right vp8Data, _) -> do
      baseImg <- decodeVP8 vp8Data
      case findAlphaChunk subChunks of
        Right alphaData -> do
          alphaVec <- decodeAlpha (imageWidth baseImg) (imageHeight baseImg) alphaData
          return $ ImageRGBA8 (combineRGBAlpha baseImg alphaVec)
        Left _ -> return $ ImageRGB8 baseImg
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

-- | Decode animation with proper canvas compositing
-- Returns composited frames as RGBA8 images
decodeAnimationWithCompositing :: Int -> Int -> WebPFile -> Either String [Image PixelRGBA8]
decodeAnimationWithCompositing canvasWidth canvasHeight (WebPExtended _header chunks) = do
  when (canvasWidth <= 0 || canvasHeight <= 0) $
    Left "Invalid canvas dimensions"
  when (canvasWidth * canvasHeight > maxCanvasPixels) $
    Left "Canvas dimensions exceed maximum supported size"
  animHeader <- findAnimHeader chunks
  anmfChunks <- findAnimFrames chunks

  let bgColor = animBackgroundColor animHeader
  composeFrames canvasWidth canvasHeight bgColor anmfChunks
decodeAnimationWithCompositing _ _ _ = Left "Not an animated WebP file"

-- | Maximum supported canvas size in pixels (2^28 ~ 1 GiB of RGBA data)
maxCanvasPixels :: Int
maxCanvasPixels = 1 `shiftL` 28

-- | Compose animation frames onto a canvas
composeFrames :: Int -> Int -> Word32 -> [(AnimFrame, [WebPChunk])] -> Either String [Image PixelRGBA8]
composeFrames canvasWidth canvasHeight bgColor frames = do
  decoded <- traverse decodeOne frames
  return $ runST $ do
    -- Create initial canvas with background color
    canvas <- VSM.replicate (canvasWidth * canvasHeight * 4) 0
    fillCanvas canvas canvasWidth canvasHeight bgColor

    let go _ [] = return []
        go prevDisposeRect ((frame, webpFrame) : rest) = do
          -- Apply disposal method from previous frame (its rectangle only)
          case prevDisposeRect of
            Just (px, py, pw, ph) -> fillRect canvas canvasWidth bgColor px py pw ph
            Nothing -> return ()

          let frameImg = webpFrameImage webpFrame
              x = anmfX frame
              y = anmfY frame
              blend = anmfBlend frame

          -- Composite frame onto canvas
          compositeFrame canvas canvasWidth canvasHeight frameImg x y blend

          -- Freeze canvas to create output image
          frozenCanvas <- VS.freeze canvas
          let outputImg = Image canvasWidth canvasHeight frozenCanvas :: Image PixelRGBA8
              disposeRect =
                if anmfDispose frame
                  then Just (x, y, dynWidth frameImg, dynHeight frameImg)
                  else Nothing

          restImgs <- go disposeRect rest
          return (outputImg : restImgs)

    go Nothing (catMaybes decoded)
  where
    decodeOne (frame, subChunks) =
      case decodeAnimFrame (frame, subChunks) of
        Left _ -> Right Nothing -- Skip frames that fail to decode
        Right webpFrame ->
          let img = webpFrameImage webpFrame
              x = anmfX frame
              y = anmfY frame
           in if x < 0
                || y < 0
                || x + dynWidth img > canvasWidth
                || y + dynHeight img > canvasHeight
                then Left "Animation frame exceeds canvas bounds"
                else Right (Just (frame, webpFrame))

dynWidth, dynHeight :: DynamicImage -> Int
dynWidth = dynamicMap imageWidth
dynHeight = dynamicMap imageHeight

-- | Fill canvas with background color
fillCanvas :: VSM.MVector s Word8 -> Int -> Int -> Word32 -> ST s ()
fillCanvas canvas width height bgColor =
  fillRect canvas width bgColor 0 0 width height

-- | Fill a rectangle of the canvas with the background color
fillRect :: VSM.MVector s Word8 -> Int -> Word32 -> Int -> Int -> Int -> Int -> ST s ()
fillRect canvas canvasWidth bgColor rx ry rw rh = do
  let b = fromIntegral (bgColor .&. 0xFF)
      g = fromIntegral ((bgColor `shiftR` 8) .&. 0xFF)
      r = fromIntegral ((bgColor `shiftR` 16) .&. 0xFF)
      a = fromIntegral ((bgColor `shiftR` 24) .&. 0xFF)

  forM_ [ry .. ry + rh - 1] $ \y ->
    forM_ [rx .. rx + rw - 1] $ \x -> do
      let i = (y * canvasWidth + x) * 4
      VSM.write canvas i r
      VSM.write canvas (i + 1) g
      VSM.write canvas (i + 2) b
      VSM.write canvas (i + 3) a

-- | Composite a frame onto the canvas
compositeFrame :: VSM.MVector s Word8 -> Int -> Int -> DynamicImage -> Int -> Int -> Bool -> ST s ()
compositeFrame canvas canvasWidth _canvasHeight frameImg frameX frameY useBlend = do
  case frameImg of
    ImageRGBA8 img -> compositeRGBA8 canvas canvasWidth img frameX frameY useBlend
    ImageRGB8 img -> compositeRGB8 canvas canvasWidth img frameX frameY useBlend
    _ -> return () -- Unsupported format, skip

-- | Composite RGBA8 image onto canvas
-- Optimized with pre-computed row bases to avoid repeated multiplication
{-# INLINE compositeRGBA8 #-}
compositeRGBA8 :: VSM.MVector s Word8 -> Int -> Image PixelRGBA8 -> Int -> Int -> Bool -> ST s ()
compositeRGBA8 canvas canvasWidth (Image frameWidth frameHeight frameData) frameX frameY useBlend = do
  forM_ [0 .. frameHeight - 1] $ \fy -> do
    let !frameRowBase = fy * frameWidth * 4
        !canvasRowBase = ((frameY + fy) * canvasWidth + frameX) * 4
    forM_ [0 .. frameWidth - 1] $ \fx -> do
      let !frameIdx = frameRowBase + fx * 4
          !canvasIdx = canvasRowBase + fx * 4

          -- Read source pixel using unsafeIndex for speed
          !srcR = frameData `VS.unsafeIndex` frameIdx
          !srcG = frameData `VS.unsafeIndex` (frameIdx + 1)
          !srcB = frameData `VS.unsafeIndex` (frameIdx + 2)
          !srcA = frameData `VS.unsafeIndex` (frameIdx + 3)

      if useBlend && srcA < 255
        then do
          -- Alpha blend
          !dstR <- VSM.unsafeRead canvas canvasIdx
          !dstG <- VSM.unsafeRead canvas (canvasIdx + 1)
          !dstB <- VSM.unsafeRead canvas (canvasIdx + 2)
          !dstA <- VSM.unsafeRead canvas (canvasIdx + 3)

          let (!blendR, !blendG, !blendB, !blendA) = alphaBlend srcR srcG srcB srcA dstR dstG dstB dstA

          VSM.unsafeWrite canvas canvasIdx blendR
          VSM.unsafeWrite canvas (canvasIdx + 1) blendG
          VSM.unsafeWrite canvas (canvasIdx + 2) blendB
          VSM.unsafeWrite canvas (canvasIdx + 3) blendA
        else do
          -- Direct copy (no blend)
          VSM.unsafeWrite canvas canvasIdx srcR
          VSM.unsafeWrite canvas (canvasIdx + 1) srcG
          VSM.unsafeWrite canvas (canvasIdx + 2) srcB
          VSM.unsafeWrite canvas (canvasIdx + 3) srcA

-- | Composite RGB8 image onto canvas (assume alpha=255)
-- Optimized with pre-computed row bases
{-# INLINE compositeRGB8 #-}
compositeRGB8 :: VSM.MVector s Word8 -> Int -> Image PixelRGB8 -> Int -> Int -> Bool -> ST s ()
compositeRGB8 canvas canvasWidth (Image frameWidth frameHeight frameData) frameX frameY _useBlend = do
  forM_ [0 .. frameHeight - 1] $ \fy -> do
    let !frameRowBase = fy * frameWidth * 3
        !canvasRowBase = ((frameY + fy) * canvasWidth + frameX) * 4
    forM_ [0 .. frameWidth - 1] $ \fx -> do
      let !frameIdx = frameRowBase + fx * 3
          !canvasIdx = canvasRowBase + fx * 4

          -- Read source pixel (RGB, assume A=255)
          !srcR = frameData `VS.unsafeIndex` frameIdx
          !srcG = frameData `VS.unsafeIndex` (frameIdx + 1)
          !srcB = frameData `VS.unsafeIndex` (frameIdx + 2)

      -- Direct copy (opaque pixels)
      VSM.unsafeWrite canvas canvasIdx srcR
      VSM.unsafeWrite canvas (canvasIdx + 1) srcG
      VSM.unsafeWrite canvas (canvasIdx + 2) srcB
      VSM.unsafeWrite canvas (canvasIdx + 3) 255

-- | Alpha blending per WebP spec
{-# INLINE alphaBlend #-}
alphaBlend :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8)
alphaBlend srcR srcG srcB srcA dstR dstG dstB dstA =
  let srcA' = fromIntegral srcA :: Int
      dstA' = fromIntegral dstA :: Int
      srcR' = fromIntegral srcR :: Int
      srcG' = fromIntegral srcG :: Int
      srcB' = fromIntegral srcB :: Int
      dstR' = fromIntegral dstR :: Int
      dstG' = fromIntegral dstG :: Int
      dstB' = fromIntegral dstB :: Int

      -- blend.A = src.A + dst.A * (1 - src.A / 255)
      blendA = srcA' + (dstA' * (255 - srcA')) `div` 255

      -- if blend.A = 0 then blend.RGB = 0
      -- else blend.RGB = (src.RGB * src.A + dst.RGB * dst.A * (1 - src.A / 255)) / blend.A
      (blendR, blendG, blendB) =
        if blendA == 0
          then (0, 0, 0)
          else
            let numeratorR = srcR' * srcA' + dstR' * dstA' * (255 - srcA') `div` 255
                numeratorG = srcG' * srcA' + dstG' * dstA' * (255 - srcA') `div` 255
                numeratorB = srcB' * srcA' + dstB' * dstA' * (255 - srcA') `div` 255
             in ( fromIntegral (numeratorR `div` blendA),
                  fromIntegral (numeratorG `div` blendA),
                  fromIntegral (numeratorB `div` blendA)
                )
   in (blendR, blendG, blendB, fromIntegral blendA)

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
