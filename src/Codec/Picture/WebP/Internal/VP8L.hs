{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8L
  ( decodeVP8L,
    decodeVP8LHeaderless,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.BitReader
import Codec.Picture.WebP.Internal.VP8L.LZ77
import Codec.Picture.WebP.Internal.VP8L.PrefixCode
import Codec.Picture.WebP.Internal.VP8L.Transform
import Control.Monad (foldM, replicateM, when)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Decode a VP8L lossless WebP image
decodeVP8L :: B.ByteString -> Either String (Image PixelRGBA8)
decodeVP8L bs = do
  let reader = initBitReader bs

  let (signature, reader1) = readBits 8 reader
  when (signature /= 0x2F) $
    Left $
      "Invalid VP8L signature: " ++ show signature

  let (widthMinus1, reader2) = readBits 14 reader1
      (heightMinus1, reader3) = readBits 14 reader2
      width = fromIntegral widthMinus1 + 1
      height = fromIntegral heightMinus1 + 1

  let (alphaIsUsed, reader4) = readBit reader3
      (versionNum, reader5) = readBits 3 reader4

  when (versionNum /= 0) $
    Left $
      "Unsupported VP8L version: " ++ show versionNum

  decodeVP8LImage width height alphaIsUsed reader5

-- | Decode a VP8L image without header (for ALPH chunk)
decodeVP8LHeaderless :: Int -> Int -> B.ByteString -> Either String (VS.Vector Word32)
decodeVP8LHeaderless width height bs = do
  let reader = initBitReader bs
  (pixels, _) <- decodeVP8LImageData width height reader []
  return pixels

-- | Decode the VP8L image data (common path for full and headless)
decodeVP8LImage :: Int -> Int -> Bool -> BitReader -> Either String (Image PixelRGBA8)
decodeVP8LImage width height alphaIsUsed reader = do
  when (width <= 0 || width > 16384) $
    Left $ "Invalid width in decodeVP8LImage: " ++ show width
  when (height <= 0 || height > 16384) $
    Left $ "Invalid height in decodeVP8LImage: " ++ show height

  (transforms, reader1) <- readTransforms width height reader

  (pixels, _) <- decodeVP8LImageData width height reader1 transforms

  finalPixels <- applyInverseTransforms transforms width height pixels

  let image = pixelsToImage width height finalPixels alphaIsUsed
  return image

-- | Read all transforms
readTransforms :: Int -> Int -> BitReader -> Either String ([VP8LTransform], BitReader)
readTransforms width height reader = go [] reader
  where
    go !acc !r = do
      let (hasTransform, r1) = readBit r
      if not hasTransform
        then return (reverse acc, r1)
        else do
          (transform, r2) <- readTransform width height r1
          go (transform : acc) r2

-- | Read a single transform
readTransform :: Int -> Int -> BitReader -> Either String (VP8LTransform, BitReader)
readTransform width height reader = do
  let (transformType, reader1) = readBits 2 reader

  case transformType of
    0 -> do
      let (sizeBits, reader2) = readBits 3 reader1
          blockSize = 1 `shiftL` fromIntegral sizeBits
          transformWidth = (width + blockSize - 1) `shiftR` fromIntegral sizeBits
          transformHeight = (height + blockSize - 1) `shiftR` fromIntegral sizeBits

      (transformData, reader3) <- decodeSubresolutionImage transformWidth transformHeight reader2

      return (TransformPredictor (fromIntegral sizeBits) transformData, reader3)
    1 -> do
      let (sizeBits, reader2) = readBits 3 reader1
          blockSize = 1 `shiftL` fromIntegral sizeBits
          transformWidth = (width + blockSize - 1) `shiftR` fromIntegral sizeBits
          transformHeight = (height + blockSize - 1) `shiftR` fromIntegral sizeBits

      (transformData, reader3) <- decodeSubresolutionImage transformWidth transformHeight reader2

      return (TransformColor (fromIntegral sizeBits) transformData, reader3)
    2 -> return (TransformSubGreen, reader1)
    3 -> do
      let (numColors, reader2) = readBits 8 reader1
          paletteSize = fromIntegral numColors + 1

      (paletteData, reader3) <- decodeSubresolutionImage paletteSize 1 reader2

      let palette = applySubtractionCoding paletteData paletteSize

      let widthBits = if width < 2 then 3 else if width < 4 then 2 else if width < 16 then 1 else 0

      return (TransformColorIndex palette widthBits, reader3)
    _ -> Left $ "Unknown transform type: " ++ show transformType

-- | Decode a subresolution image (no transforms, single prefix code group)
-- Per RFC 9649: subresolution images have NO transforms and NO meta prefix codes
decodeSubresolutionImage :: Int -> Int -> BitReader -> Either String (VS.Vector Word32, BitReader)
decodeSubresolutionImage width height reader = do
  -- Subresolution images: read color cache, then single prefix code group, then LZ77
  let (usesColorCache, reader1) = readBit reader

  (maybeCache, reader2) <-
    if usesColorCache
      then do
        let (cacheBits, r) = readBits 4 reader1
        when (cacheBits < 1 || cacheBits > 11) $
          Left $
            "Invalid color cache bits: " ++ show cacheBits
        return (Just $ createColorCache (fromIntegral cacheBits), r)
      else return (Nothing, reader1)

  -- Subresolution images always use a single prefix code group (no meta prefix codes bit)
  (group, reader3) <- readPrefixCodeGroup reader2 maybeCache

  -- Decode LZ77 data
  decodeLZ77 width height maybeCache group Nothing reader3

-- | Decode spatially-coded image (main image or entropy image)
decodeVP8LImageData :: Int -> Int -> BitReader -> [VP8LTransform] -> Either String (VS.Vector Word32, BitReader)
decodeVP8LImageData width height reader _transforms = do
  let (usesColorCache, reader1) = readBit reader

  (maybeCache, reader2) <-
    if usesColorCache
      then do
        let (cacheBits, r) = readBits 4 reader1
        when (cacheBits < 1 || cacheBits > 11) $
          Left $
            "Invalid color cache bits: " ++ show cacheBits
        return (Just $ createColorCache (fromIntegral cacheBits), r)
      else return (Nothing, reader1)

  let (usesMetaPrefixCodes, reader3) = readBit reader2

  (prefixCodeGroups, entropyImage, reader4) <-
    if usesMetaPrefixCodes
      then do
        let (prefixBits, r1) = readBits 3 reader3
            entropyWidth = (width + (1 `shiftL` fromIntegral prefixBits) - 1) `shiftR` fromIntegral prefixBits
            entropyHeight = (height + (1 `shiftL` fromIntegral prefixBits) - 1) `shiftR` fromIntegral prefixBits

        (entropyImg, r2) <- decodeSubresolutionImage entropyWidth entropyHeight r1

        let numGroups = countEntropyGroups entropyImg

        (groupList, finalReader) <- readMultiplePrefixCodeGroups numGroups maybeCache r2

        return (groupList, Just (entropyImg, fromIntegral prefixBits), finalReader)
      else do
        (group, r) <- readPrefixCodeGroup reader3 maybeCache
        return ([group], Nothing, r)

  codeGroup <- case prefixCodeGroups of
    [] -> Left "No prefix code groups"
    (g : _) -> Right g

  decodeLZ77 width height maybeCache codeGroup entropyImage reader4

-- | Count the number of entropy groups in the entropy image
countEntropyGroups :: VS.Vector Word32 -> Int
countEntropyGroups entropyImage =
  let maxGroup = VS.foldl' (\acc pixel -> max acc (fromIntegral ((pixel `shiftR` 8) .&. 0xFF))) 0 entropyImage
   in maxGroup + 1

-- | Read multiple prefix code groups sequentially
readMultiplePrefixCodeGroups :: Int -> Maybe ColorCache -> BitReader -> Either String ([PrefixCodeGroup], BitReader)
readMultiplePrefixCodeGroups n maybeCache reader = go [] n reader
  where
    go !acc !remaining !r
      | remaining <= 0 = return (reverse acc, r)
      | otherwise = do
          (group, r') <- readPrefixCodeGroup r maybeCache
          go (group : acc) (remaining - 1) r'

-- | Read a prefix code group (5 codes)
readPrefixCodeGroup :: BitReader -> Maybe ColorCache -> Either String (PrefixCodeGroup, BitReader)
readPrefixCodeGroup reader maybeCache = do
  let cacheSize = case maybeCache of
        Nothing -> 0
        Just cache -> 1 `shiftL` ccBits cache

  (greenCode, reader1) <- readPrefixCodeWithAlphabet (256 + 24 + cacheSize) reader
  (redCode, reader2) <- readPrefixCodeWithAlphabet 256 reader1
  (blueCode, reader3) <- readPrefixCodeWithAlphabet 256 reader2
  (alphaCode, reader4) <- readPrefixCodeWithAlphabet 256 reader3
  (distCode, reader5) <- readPrefixCodeWithAlphabet 40 reader4

  return
    ( PrefixCodeGroup
        { pcgGreen = greenCode,
          pcgRed = redCode,
          pcgBlue = blueCode,
          pcgAlpha = alphaCode,
          pcgDistance = distCode
        },
      reader5
    )

-- | Read a single prefix code with given alphabet size
readPrefixCodeWithAlphabet :: Int -> BitReader -> Either String (PrefixCode, BitReader)
readPrefixCodeWithAlphabet alphabetSize reader = do
  (codeLengths, reader1) <- readCodeLengths alphabetSize reader

  case buildPrefixCode codeLengths of
    Left err -> Left $ "Failed to build prefix code for alphabet size " ++ show alphabetSize ++ ": " ++ err
    Right code -> return (code, reader1)

-- | Apply subtraction coding to palette data
applySubtractionCoding :: VS.Vector Word32 -> Int -> VS.Vector Word32
applySubtractionCoding palette size = VS.generate size $ \i ->
  if i == 0
    then palette VS.! 0
    else
      let prev = palette VS.! (i - 1)
          curr = palette VS.! i

          prevA = (prev `shiftR` 24) .&. 0xFF
          prevR = (prev `shiftR` 16) .&. 0xFF
          prevG = (prev `shiftR` 8) .&. 0xFF
          prevB = prev .&. 0xFF

          currA = (curr `shiftR` 24) .&. 0xFF
          currR = (curr `shiftR` 16) .&. 0xFF
          currG = (curr `shiftR` 8) .&. 0xFF
          currB = curr .&. 0xFF

          newA = (currA + prevA) .&. 0xFF
          newR = (currR + prevR) .&. 0xFF
          newG = (currG + prevG) .&. 0xFF
          newB = (currB + prevB) .&. 0xFF
       in (newA `shiftL` 24) .|. (newR `shiftL` 16) .|. (newG `shiftL` 8) .|. newB

-- | Convert pixel data to JuicyPixels image
pixelsToImage :: Int -> Int -> VS.Vector Word32 -> Bool -> Image PixelRGBA8
pixelsToImage width height pixels alphaIsUsed =
  let totalComponents = width * height * 4
      pixelData = VS.generate totalComponents $ \i ->
        let pixelIdx = i `div` 4
            component = i `mod` 4
         in if pixelIdx < 0 || pixelIdx >= VS.length pixels
              then error $ "Pixel index out of bounds: " ++ show pixelIdx ++ " (pixels length: " ++ show (VS.length pixels) ++ ", i=" ++ show i ++ ")"
              else
                let pixel = pixels VS.! pixelIdx
                 in case component of
                      0 -> fromIntegral ((pixel `shiftR` 16) .&. 0xFF)  -- R
                      1 -> fromIntegral ((pixel `shiftR` 8) .&. 0xFF)   -- G
                      2 -> fromIntegral (pixel .&. 0xFF)                  -- B
                      3 -> if alphaIsUsed then fromIntegral ((pixel `shiftR` 24) .&. 0xFF) else 255  -- A
                      _ -> 0
   in Image width height pixelData
