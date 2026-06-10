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
import qualified Data.Vector as V
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

  let (_alphaIsUsed, reader4) = readBit reader3
      (versionNum, reader5) = readBits 3 reader4

  when (versionNum /= 0) $
    Left $
      "Unsupported VP8L version: " ++ show versionNum

  decodeVP8LImage width height reader5

-- | Decode a VP8L image without header (for ALPH chunk).
-- The stream starts at the transform-present flag.
decodeVP8LHeaderless :: Int -> Int -> B.ByteString -> Either String (VS.Vector Word32)
decodeVP8LHeaderless width height bs = do
  let reader = initBitReader bs
  (transforms, effectiveWidth, reader1) <- readTransforms width height reader
  (pixels, _) <- decodeVP8LImageData effectiveWidth height reader1 transforms
  applyInverseTransforms transforms width height pixels

-- | Decode the VP8L image data (common path for full and headless)
decodeVP8LImage :: Int -> Int -> BitReader -> Either String (Image PixelRGBA8)
decodeVP8LImage width height reader = do
  when (width <= 0 || width > 16384) $
    Left $
      "Invalid width in decodeVP8LImage: " ++ show width
  when (height <= 0 || height > 16384) $
    Left $
      "Invalid height in decodeVP8LImage: " ++ show height

  (transforms, effectiveWidth, reader1) <- readTransforms width height reader

  (pixels, _) <- decodeVP8LImageData effectiveWidth height reader1 transforms

  finalPixels <- applyInverseTransforms transforms width height pixels

  let image = pixelsToImage width height finalPixels
  return image

-- | Read all transforms, returning the effective width (may be reduced by color-indexing bundling)
readTransforms :: Int -> Int -> BitReader -> Either String ([VP8LTransform], Int, BitReader)
readTransforms width height reader = go [] width reader
  where
    go !acc !w !r = do
      let (hasTransform, r1) = readBit r
      if not hasTransform
        then return (reverse acc, w, r1)
        else do
          (transform, r2) <- readTransform w height r1
          -- Color-indexing with bundling reduces the effective image width
          let w' = case transform of
                TransformColorIndex _ wb
                  | wb > 0 -> (w + (1 `shiftL` wb) - 1) `shiftR` wb
                _ -> w
          go (transform : acc) w' r2

-- | Read a single transform
readTransform :: Int -> Int -> BitReader -> Either String (VP8LTransform, BitReader)
readTransform width height reader = do
  let (transformType, reader1) = readBits 2 reader

  case transformType of
    0 -> do
      let (sizeBitsRaw, reader2) = readBits 3 reader1
          sizeBits = sizeBitsRaw + 2 -- spec: size_bits = ReadBits(3) + 2
          blockSize = 1 `shiftL` fromIntegral sizeBits
          transformWidth = (width + blockSize - 1) `shiftR` fromIntegral sizeBits
          transformHeight = (height + blockSize - 1) `shiftR` fromIntegral sizeBits

      (transformData, reader3) <- decodeSubresolutionImage transformWidth transformHeight reader2

      return (TransformPredictor (fromIntegral sizeBits) transformData, reader3)
    1 -> do
      let (sizeBitsRaw, reader2) = readBits 3 reader1
          sizeBits = sizeBitsRaw + 2 -- spec: size_bits = ReadBits(3) + 2
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

      let widthBits = if paletteSize <= 2 then 3 else if paletteSize <= 4 then 2 else if paletteSize <= 16 then 1 else 0

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
  decodeLZ77 width height maybeCache (V.singleton group) Nothing reader3

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

  when (null prefixCodeGroups) $
    Left "No prefix code groups"

  decodeLZ77 width height maybeCache (V.fromList prefixCodeGroups) entropyImage reader4

-- | Count the number of entropy groups in the entropy image.
-- The meta prefix code is the 16-bit value (red << 8) | green of each entropy pixel.
{-# INLINE countEntropyGroups #-}
countEntropyGroups :: VS.Vector Word32 -> Int
countEntropyGroups entropyImage =
  let maxGroup = VS.foldl' (\acc pixel -> max acc (fromIntegral ((pixel `shiftR` 8) .&. 0xFFFF))) 0 entropyImage
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

-- | Apply inverse subtraction coding to palette data (cumulative addition).
-- stored[i] contains a delta; actual[i] = stored[i] + actual[i-1] per channel, mod 256.
applySubtractionCoding :: VS.Vector Word32 -> Int -> VS.Vector Word32
applySubtractionCoding stored size
  | size <= 0 = VS.empty
  | otherwise = VS.constructN size $ \built ->
      let !i = VS.length built
       in if i == 0
            then stored VS.! 0
            else addPixels (stored VS.! i) (built `VS.unsafeIndex` (i - 1))

-- | Add two pixels component-wise (mod 256)
{-# INLINE addPixels #-}
addPixels :: Word32 -> Word32 -> Word32
addPixels p1 p2 =
  let !a = (((p1 `shiftR` 24) .&. 0xFF) + ((p2 `shiftR` 24) .&. 0xFF)) .&. 0xFF
      !r = (((p1 `shiftR` 16) .&. 0xFF) + ((p2 `shiftR` 16) .&. 0xFF)) .&. 0xFF
      !g = (((p1 `shiftR` 8) .&. 0xFF) + ((p2 `shiftR` 8) .&. 0xFF)) .&. 0xFF
      !b = ((p1 .&. 0xFF) + (p2 .&. 0xFF)) .&. 0xFF
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

-- | Convert pixel data to JuicyPixels image.
-- The alpha_is_used header bit is only a hint and must not affect decoding.
{-# INLINE pixelsToImage #-}
pixelsToImage :: Int -> Int -> VS.Vector Word32 -> Image PixelRGBA8
pixelsToImage width height pixels =
  let totalComponents = width * height * 4
      !pixelsLen = VS.length pixels
      pixelData = VS.generate totalComponents $ \i ->
        let !pixelIdx = i `shiftR` 2 -- i `div` 4
            !component = i .&. 3 -- i `mod` 4
         in if pixelIdx < 0 || pixelIdx >= pixelsLen
              then error $ "Pixel index out of bounds: " ++ show pixelIdx ++ " (pixels length: " ++ show pixelsLen ++ ", i=" ++ show i ++ ")"
              else
                let pixel = pixels `VS.unsafeIndex` pixelIdx
                 in case component of
                      0 -> fromIntegral ((pixel `shiftR` 16) .&. 0xFF) -- R
                      1 -> fromIntegral ((pixel `shiftR` 8) .&. 0xFF) -- G
                      2 -> fromIntegral (pixel .&. 0xFF) -- B
                      3 -> fromIntegral ((pixel `shiftR` 24) .&. 0xFF) -- A
                      _ -> 0
   in Image width height pixelData
