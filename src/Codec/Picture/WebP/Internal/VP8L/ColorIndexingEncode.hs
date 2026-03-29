{-# LANGUAGE BangPatterns #-}

-- | VP8L color-indexing transform encoder.
-- For images with ≤ 256 unique colors, replaces each pixel with a palette index.
-- For palettes ≤ 16 entries, multiple indices are packed into one pixel (pixel bundling).
module Codec.Picture.WebP.Internal.VP8L.ColorIndexingEncode
  ( ColorIndexResult (..),
    tryColorIndexing,
  )
where

import Data.Bits
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector.Storable as VS
import Data.Word

-- | Result of applying the color-indexing forward transform
data ColorIndexResult = ColorIndexResult
  { -- | Subtraction-coded palette (for writing to bitstream)
    ciPalette :: !(VS.Vector Word32),
    -- | Indexed (and optionally bundled) pixels
    ciIndexedPixels :: !(VS.Vector Word32),
    -- | Image width after pixel bundling
    ciPackedWidth :: !Int,
    -- | Bundling width bits (0-3)
    ciWidthBits :: !Int,
    -- | Number of palette entries (1-256)
    ciPaletteSize :: !Int
  }
  deriving (Show)

-- | Try to apply the color-indexing transform. Returns Nothing if > 256 unique colors.
tryColorIndexing :: Int -> Int -> VS.Vector Word32 -> Maybe ColorIndexResult
tryColorIndexing width height pixels
  | VS.null pixels = Nothing
  | otherwise = case collectUniqueColors pixels of
      Nothing -> Nothing
      Just uniqueSet -> Just $ buildColorIndex width height pixels uniqueSet

-- | Collect unique ARGB colors, returning Nothing if more than 256.
collectUniqueColors :: VS.Vector Word32 -> Maybe IS.IntSet
collectUniqueColors pixels = go IS.empty 0 0
  where
    !len = VS.length pixels
    go !s !count !i
      | count > 256 = Nothing
      | i >= len = Just s
      | IS.member key s = go s count (i + 1)
      | otherwise = go (IS.insert key s) (count + 1) (i + 1)
      where
        !key = fromIntegral (pixels `VS.unsafeIndex` i)

-- | Build the color-indexing result from the set of unique colors.
buildColorIndex :: Int -> Int -> VS.Vector Word32 -> IS.IntSet -> ColorIndexResult
buildColorIndex width height pixels uniqueSet =
  let -- Build palette sorted by color value
      !paletteList = IS.toAscList uniqueSet
      !paletteSize = length paletteList
      !rawPalette = VS.fromListN paletteSize (map fromIntegral paletteList)

      -- Reverse lookup: color -> palette index
      !colorToIndex = IM.fromList (zip paletteList [0 ..])

      -- Pixel bundling parameters
      !widthBits = computeWidthBits paletteSize
      !pixelsPerByte = 1 `shiftL` widthBits
      !packedWidth = (width + pixelsPerByte - 1) `shiftR` widthBits

      -- Subtraction-code the palette for storage
      !subPalette = computeSubtractionCoding rawPalette

      -- Replace pixels with indices (and bundle if applicable)
      !indexedPixels =
        if widthBits == 0
          then indexPixelsSimple width height pixels colorToIndex
          else bundlePixels width height pixels colorToIndex widthBits packedWidth
   in ColorIndexResult
        { ciPalette = subPalette,
          ciIndexedPixels = indexedPixels,
          ciPackedWidth = packedWidth,
          ciWidthBits = widthBits,
          ciPaletteSize = paletteSize
        }

-- | Compute widthBits from palette size (RFC 9649 Section 4.2.4).
{-# INLINE computeWidthBits #-}
computeWidthBits :: Int -> Int
computeWidthBits ps
  | ps <= 2 = 3 -- 1 bit/pixel, 8 pixels/byte
  | ps <= 4 = 2 -- 2 bits/pixel, 4 pixels/byte
  | ps <= 16 = 1 -- 4 bits/pixel, 2 pixels/byte
  | otherwise = 0 -- 8 bits/pixel, no bundling

-- | Compute subtraction coding: stored[i] = raw[i] - raw[i-1] per channel, mod 256.
-- This is the inverse of the decoder's cumulative addition.
computeSubtractionCoding :: VS.Vector Word32 -> VS.Vector Word32
computeSubtractionCoding palette = VS.generate (VS.length palette) $ \i ->
  if i == 0
    then palette `VS.unsafeIndex` 0
    else subPixels (palette `VS.unsafeIndex` i) (palette `VS.unsafeIndex` (i - 1))

-- | Index pixels without bundling (palette size 17-256).
-- Each pixel becomes: alpha=0xFF, red=0, green=index, blue=0.
indexPixelsSimple :: Int -> Int -> VS.Vector Word32 -> IM.IntMap Int -> VS.Vector Word32
indexPixelsSimple _width _height pixels colorToIndex =
  VS.map
    ( \px ->
        let !idx = IM.findWithDefault 0 (fromIntegral px) colorToIndex
         in 0xFF000000 .|. (fromIntegral idx `shiftL` 8)
    )
    pixels

-- | Bundle pixels (palette size 1-16): pack multiple indices into one green byte, LSB-first.
bundlePixels :: Int -> Int -> VS.Vector Word32 -> IM.IntMap Int -> Int -> Int -> VS.Vector Word32
bundlePixels width height pixels colorToIndex widthBits packedWidth =
  let !bitsPerPixel = 8 `shiftR` widthBits
      !pixelsPerByte = 1 `shiftL` widthBits
      !mask = (1 `shiftL` bitsPerPixel) - 1
   in VS.generate (packedWidth * height) $ \i ->
        let !py = i `div` packedWidth
            !px = i `mod` packedWidth
            !green = packGroup py px 0 (0 :: Int)

            -- Pack pixelsPerByte indices into one byte
            packGroup !row !col !j !acc
              | j >= pixelsPerByte = acc
              | otherwise =
                  let !origX = col * pixelsPerByte + j
                      !origI = row * width + origX
                      !idx =
                        if origX < width
                          then IM.findWithDefault 0 (fromIntegral (pixels `VS.unsafeIndex` origI)) colorToIndex .&. mask
                          else 0
                   in packGroup row col (j + 1) (acc .|. (idx `shiftL` (j * bitsPerPixel)))
         in 0xFF000000 .|. (fromIntegral green `shiftL` 8)

-- | Subtract two pixels component-wise (mod 256)
{-# INLINE subPixels #-}
subPixels :: Word32 -> Word32 -> Word32
subPixels p1 p2 =
  let !a = ((p1 `shiftR` 24) - (p2 `shiftR` 24)) .&. 0xFF
      !r = (((p1 `shiftR` 16) .&. 0xFF) - ((p2 `shiftR` 16) .&. 0xFF)) .&. 0xFF
      !g = (((p1 `shiftR` 8) .&. 0xFF) - ((p2 `shiftR` 8) .&. 0xFF)) .&. 0xFF
      !b = ((p1 .&. 0xFF) - (p2 .&. 0xFF)) .&. 0xFF
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b
