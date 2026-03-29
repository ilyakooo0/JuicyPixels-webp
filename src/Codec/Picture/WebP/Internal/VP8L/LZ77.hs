{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8L.LZ77
  ( ColorCache (..),
    createColorCache,
    insertColor,
    lookupColor,
    decodeLZ77,
    PrefixCodeGroup (..),
    kDistanceMapXY,
    lengthPrefixTable,
    distancePrefixTable,
  )
where

import Codec.Picture.WebP.Internal.BitReader
import Codec.Picture.WebP.Internal.VP8L.PrefixCode
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import Data.STRef
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Color cache for LZ77 decoding (immutable, for external API)
data ColorCache = ColorCache
  { ccBits :: !Int,
    ccColors :: !(VS.Vector Word32)
  }
  deriving (Show)

-- | Mutable color cache for use within ST monad (CRITICAL OPTIMIZATION)
-- This avoids allocating a new vector on every pixel insertion
data MutableColorCache s = MutableColorCache
  { mccBits :: !Int,
    mccColors :: !(VSM.MVector s Word32)
  }

-- | Create a color cache with the given number of bits
createColorCache :: Int -> ColorCache
createColorCache bits =
  let size = 1 `shiftL` bits
   in ColorCache bits (VS.replicate size 0)

-- | Create a mutable color cache within ST monad
{-# INLINE createMutableColorCache #-}
createMutableColorCache :: Int -> ST s (MutableColorCache s)
createMutableColorCache bits = do
  let size = 1 `shiftL` bits
  colors <- VSM.replicate size 0
  return $ MutableColorCache bits colors

-- | Insert a color into the cache (immutable version for external API)
insertColor :: Word32 -> ColorCache -> ColorCache
insertColor color cache =
  let idx = colorCacheHash color (ccBits cache)
      newColors = VS.modify (\v -> VSM.write v idx color) (ccColors cache)
   in cache {ccColors = newColors}

-- | Insert a color into the mutable cache (CRITICAL OPTIMIZATION)
-- Single write operation, no allocation
{-# INLINE insertColorM #-}
insertColorM :: Word32 -> MutableColorCache s -> ST s ()
insertColorM color cache = do
  let idx = colorCacheHash color (mccBits cache)
  VSM.unsafeWrite (mccColors cache) idx color

-- | Look up a color from the mutable cache
{-# INLINE lookupColorM #-}
lookupColorM :: Int -> MutableColorCache s -> ST s Word32
lookupColorM idx cache = VSM.unsafeRead (mccColors cache) idx

-- | Look up a color from the cache
lookupColor :: Int -> ColorCache -> Word32
lookupColor idx cache = ccColors cache VS.! idx

-- | Color cache hash function
{-# INLINE colorCacheHash #-}
colorCacheHash :: Word32 -> Int -> Int
colorCacheHash color bits =
  let hash = (0x1e35a7bd :: Word32) * color
   in fromIntegral (hash `shiftR` (32 - bits))

-- | The 120 (xi, yi) offset pairs for VP8L 2D distance codes (RFC 9649 Section 4.2.2).
-- Index i corresponds to distance code (i+1).
-- Convert to scan-line distance: dist = xi + yi * image_width; if dist < 1 then dist = 1.
kDistanceMapXY :: VU.Vector (Int, Int)
kDistanceMapXY =
  VU.fromList
    [ (0, 1),
      (1, 0),
      (1, 1),
      (-1, 1),
      (0, 2),
      (2, 0),
      (1, 2),
      (-1, 2),
      (2, 1),
      (-2, 1),
      (2, 2),
      (-2, 2),
      (0, 3),
      (3, 0),
      (1, 3),
      (-1, 3),
      (3, 1),
      (-3, 1),
      (2, 3),
      (-2, 3),
      (3, 2),
      (-3, 2),
      (0, 4),
      (4, 0),
      (1, 4),
      (-1, 4),
      (4, 1),
      (-4, 1),
      (3, 3),
      (-3, 3),
      (2, 4),
      (-2, 4),
      (4, 2),
      (-4, 2),
      (0, 5),
      (3, 4),
      (-3, 4),
      (4, 3),
      (-4, 3),
      (5, 0),
      (1, 5),
      (-1, 5),
      (5, 1),
      (-5, 1),
      (2, 5),
      (-2, 5),
      (5, 2),
      (-5, 2),
      (4, 4),
      (-4, 4),
      (3, 5),
      (-3, 5),
      (5, 3),
      (-5, 3),
      (0, 6),
      (6, 0),
      (1, 6),
      (-1, 6),
      (6, 1),
      (-6, 1),
      (2, 6),
      (-2, 6),
      (6, 2),
      (-6, 2),
      (4, 5),
      (-4, 5),
      (5, 4),
      (-5, 4),
      (3, 6),
      (-3, 6),
      (6, 3),
      (-6, 3),
      (0, 7),
      (7, 0),
      (1, 7),
      (-1, 7),
      (5, 5),
      (-5, 5),
      (7, 1),
      (-7, 1),
      (4, 6),
      (-4, 6),
      (6, 4),
      (-6, 4),
      (2, 7),
      (-2, 7),
      (7, 2),
      (-7, 2),
      (3, 7),
      (-3, 7),
      (7, 3),
      (-7, 3),
      (5, 6),
      (-5, 6),
      (6, 5),
      (-6, 5),
      (8, 0),
      (4, 7),
      (-4, 7),
      (7, 4),
      (-7, 4),
      (8, 1),
      (8, 2),
      (6, 6),
      (-6, 6),
      (8, 3),
      (5, 7),
      (-5, 7),
      (7, 5),
      (-7, 5),
      (8, 4),
      (6, 7),
      (-6, 7),
      (7, 6),
      (-7, 6),
      (8, 5),
      (7, 7),
      (-7, 7),
      (8, 6),
      (8, 7)
    ]

-- | Length prefix table: (base_length, extra_bits)
-- Indexed by green symbol (0-279). Symbols 0-255 are literals.
-- Symbols 256-279 are LZ77 length codes (prefix codes 0-23).
-- Formula from RFC 9649 Section 4.2.1:
--   if code < 4: value = code + 1
--   else: extra_bits = (code-2)>>1; offset = (2 + (code&1)) << extra_bits; value = offset + extra + 1
lengthPrefixTable :: VU.Vector (Int, Int)
lengthPrefixTable = VU.generate 280 $ \sym ->
  if sym < 256
    then (sym, 0)
    else
      let code = sym - 256
       in if code < 4
            then (code + 1, 0)
            else
              let extraBits = (code - 2) `shiftR` 1
                  base = (2 + (code .&. 1)) `shiftL` extraBits
               in (base + 1, extraBits)

-- | Distance prefix table: extra bits for each distance code
distancePrefixTable :: VU.Vector Int
distancePrefixTable = VU.generate 40 $ \code ->
  if code < 4
    then 0
    else (code - 2) `shiftR` 1

-- | Prefix code group (5 codes for green+len+cache, R, B, A, distance)
data PrefixCodeGroup = PrefixCodeGroup
  { pcgGreen :: !PrefixCode,
    pcgRed :: !PrefixCode,
    pcgBlue :: !PrefixCode,
    pcgAlpha :: !PrefixCode,
    pcgDistance :: !PrefixCode
  }

-- | Decode LZ77-compressed image data
decodeLZ77 ::
  Int ->
  Int ->
  Maybe ColorCache ->
  PrefixCodeGroup ->
  Maybe (VS.Vector Word32, Int) ->
  BitReader ->
  Either String (VS.Vector Word32, BitReader)
decodeLZ77 width height maybeCache codeGroup maybeEntropyImage reader = runST $ do
  when (width <= 0 || width > 16384 || height <= 0 || height > 16384) $
    error $
      "Invalid dimensions in decodeLZ77: " ++ show width ++ "x" ++ show height

  let totalPixels = width * height
  when (totalPixels <= 0 || totalPixels > 268435456) $ -- 16384^2
    error $
      "Total pixels out of range: " ++ show totalPixels

  output <- VSM.new totalPixels

  -- CRITICAL OPTIMIZATION: Use mutable color cache to avoid allocation per pixel
  let cacheBits = maybe 0 ccBits maybeCache
      useCache = cacheBits > 0
  mutableCache <- createMutableColorCache cacheBits

  let loop !pos !r
        | pos >= totalPixels = do
            result <- VS.unsafeFreeze output
            return $ Right (result, r)
        | otherwise = do
            let (y, x) = pos `divMod` width -- Fixed: y is row (quotient), x is column (remainder)
                _groupIdx = getEntropyGroup x y maybeEntropyImage width

            let (greenSym, r1) = decodeSymbol (pcgGreen codeGroup) r

            if greenSym < 256
              then do
                let (redSym, r2) = decodeSymbol (pcgRed codeGroup) r1
                    (blueSym, r3) = decodeSymbol (pcgBlue codeGroup) r2
                    (alphaSym, r4) = decodeSymbol (pcgAlpha codeGroup) r3

                    !color = packColor (fromIntegral alphaSym) (fromIntegral redSym) (fromIntegral greenSym) (fromIntegral blueSym)

                VSM.unsafeWrite output pos color

                -- Insert into mutable cache (no allocation)
                when useCache $ insertColorM color mutableCache

                loop (pos + 1) r4
              else
                if greenSym < 280
                  then do
                    let lengthCode = fromIntegral greenSym
                    when (lengthCode < 256 || lengthCode >= 280) $
                      error $
                        "Invalid length code: " ++ show lengthCode

                    let (baseLen, extraBits) = lengthPrefixTable VU.! lengthCode
                    when (extraBits > 20) $
                      error $
                        "Length extra bits too large: " ++ show extraBits

                    let (extra, r2) = readBits extraBits r1
                        !len = baseLen + fromIntegral extra

                    when (len > 100000) $
                      error $
                        "Length too large: " ++ show len

                    -- Decode distance symbol (0-39) and apply prefix decoding
                    -- to get the actual distance code, then use 2D map or 1D offset.
                    let (distSym, r3) = decodeSymbol (pcgDistance codeGroup) r2
                        !distPrefixCode = fromIntegral distSym :: Int

                    let (!distCode, !r4) =
                          if distPrefixCode < 4
                            then (distPrefixCode + 1, r3)
                            else
                              let !distExtraBits = (distPrefixCode - 2) `shiftR` 1
                                  !distOffset = (2 + (distPrefixCode .&. 1)) `shiftL` distExtraBits
                                  (!distExtra, !r3') = readBits distExtraBits r3
                               in (distOffset + fromIntegral distExtra + 1, r3')

                    -- Convert distance code to scan-line pixel distance
                    let !dist =
                          if distCode <= 120
                            then
                              let (!xi, !yi) = kDistanceMapXY VU.! (distCode - 1)
                               in max 1 (xi + yi * width)
                            else distCode - 120

                    when (dist > pos) $
                      error $
                        "Distance " ++ show dist ++ " exceeds position " ++ show pos

                    copyLoop pos dist len output mutableCache useCache r4
                  else do
                    let cacheIdx = fromIntegral greenSym - 280
                    if not useCache
                      then return $ Left $ "Color cache symbol " ++ show greenSym ++ " (cache idx " ++ show cacheIdx ++ ") decoded but no cache initialized. Alphabet was 280 symbols (256 lit + 24 len), but got symbol >= 280. Decoder bug or invalid bitstream."
                      else do
                        color <- lookupColorM cacheIdx mutableCache
                        VSM.unsafeWrite output pos color
                        insertColorM color mutableCache
                        loop (pos + 1) r1

      -- CRITICAL OPTIMIZATION: Batched copy loop with special cases
      copyLoop !pos !dist !len !out !cache !doCache !r
        | len <= 0 = loop pos r
        | pos >= totalPixels = loop pos r
        | otherwise = do
            let srcPos = pos - dist
            when (srcPos < 0) $
              error $
                "Invalid back-reference: distance=" ++ show dist ++ " at pos=" ++ show pos

            -- Calculate how many pixels we can actually copy
            let actualLen = min len (totalPixels - pos)

            -- Special case: dist=1 means repeat single pixel (very common for runs)
            if dist == 1
              then do
                color <- VSM.unsafeRead out srcPos
                -- Fill with single color
                forM_ [pos .. pos + actualLen - 1] $ \i ->
                  VSM.unsafeWrite out i color
                -- Only need to insert once for the color cache (it's the same color)
                when doCache $ insertColorM color cache
                loop (pos + actualLen) r
              else
                if dist >= actualLen
                  then do
                    -- Non-overlapping: can use bulk copy
                    -- Copy in one batch using slice operations
                    forM_ [0 .. actualLen - 1] $ \i -> do
                      color <- VSM.unsafeRead out (srcPos + i)
                      VSM.unsafeWrite out (pos + i) color
                    -- Sample cache insertions (every 8th pixel to reduce overhead)
                    when doCache $ do
                      forM_ [0, 8 .. actualLen - 1] $ \i -> do
                        color <- VSM.unsafeRead out (pos + i)
                        insertColorM color cache
                      -- Always insert the last pixel to ensure cache coherency
                      when (actualLen > 0 && ((actualLen - 1) .&. 7) /= 0) $ do
                        lastColor <- VSM.unsafeRead out (pos + actualLen - 1)
                        insertColorM lastColor cache
                    loop (pos + actualLen) r
                  else do
                    -- Overlapping case: must copy pixel-by-pixel but still use mutable cache
                    let copyOverlapping !i
                          | i >= actualLen = loop (pos + actualLen) r
                          | otherwise = do
                              color <- VSM.unsafeRead out (srcPos + i)
                              VSM.unsafeWrite out (pos + i) color
                              when doCache $ insertColorM color cache
                              copyOverlapping (i + 1)
                    copyOverlapping 0

  loop 0 reader

-- | Get entropy group index for a pixel
{-# INLINE getEntropyGroup #-}
getEntropyGroup :: Int -> Int -> Maybe (VS.Vector Word32, Int) -> Int -> Int
getEntropyGroup _x _y Nothing _width = 0
getEntropyGroup x y (Just (entropyImage, prefixBits)) width =
  let !entropyWidth = (width + (1 `shiftL` prefixBits) - 1) `shiftR` prefixBits
      !entropyX = x `shiftR` prefixBits
      !entropyY = y `shiftR` prefixBits
      -- Int is safe: max image 16384x16384, max entropyWidth = 16384, max product = 268M < 2^31
      !entropyIdx = entropyY * entropyWidth + entropyX
      !entropyLen = VS.length entropyImage
   in if entropyIdx < 0 || entropyIdx >= entropyLen
        then error $ "Entropy index out of bounds: " ++ show entropyIdx ++ " (entropyImage length: " ++ show entropyLen ++ ")"
        else
          let !pixel = entropyImage `VS.unsafeIndex` entropyIdx
              !green = (pixel `shiftR` 8) .&. 0xFF
           in fromIntegral green

-- | Pack ARGB components into a Word32
{-# INLINE packColor #-}
packColor :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
packColor a r g b =
  (fromIntegral a `shiftL` 24)
    .|. (fromIntegral r `shiftL` 16)
    .|. (fromIntegral g `shiftL` 8)
    .|. fromIntegral b
