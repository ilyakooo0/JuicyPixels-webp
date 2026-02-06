{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8L.LZ77
  ( ColorCache (..),
    createColorCache,
    insertColor,
    lookupColor,
    decodeLZ77,
    PrefixCodeGroup (..),
    kDistanceMap,
    lengthPrefixTable,
    distancePrefixTable,
  )
where

import Codec.Picture.WebP.Internal.BitReader
import Codec.Picture.WebP.Internal.VP8L.PrefixCode
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Color cache for LZ77 decoding
data ColorCache = ColorCache
  { ccBits :: !Int,
    ccColors :: !(VS.Vector Word32)
  }
  deriving (Show)

-- | Create a color cache with the given number of bits
createColorCache :: Int -> ColorCache
createColorCache bits =
  let size = 1 `shiftL` bits
   in ColorCache bits (VS.replicate size 0)

-- | Insert a color into the cache
insertColor :: Word32 -> ColorCache -> ColorCache
insertColor color cache =
  let idx = colorCacheHash color (ccBits cache)
      newColors = VS.modify (\v -> VSM.write v idx color) (ccColors cache)
   in cache {ccColors = newColors}

-- | Look up a color from the cache
lookupColor :: Int -> ColorCache -> Word32
lookupColor idx cache = ccColors cache VS.! idx

-- | Color cache hash function
colorCacheHash :: Word32 -> Int -> Int
colorCacheHash color bits =
  let hash = (0x1e35a7bd :: Word32) * color
   in fromIntegral (hash `shiftR` (32 - bits))

-- | Distance map for small distance codes (1-120)
kDistanceMap :: VU.Vector Int
kDistanceMap =
  VU.fromList
    [ 0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36,
      37,
      38,
      39,
      40,
      41,
      42,
      43,
      44,
      45,
      46,
      47,
      48,
      49,
      50,
      51,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      61,
      62,
      63,
      64,
      65,
      66,
      67,
      68,
      69,
      70,
      71,
      72,
      73,
      74,
      75,
      76,
      77,
      78,
      79,
      80,
      81,
      82,
      83,
      84,
      85,
      86,
      87,
      88,
      89,
      90,
      91,
      92,
      93,
      94,
      95,
      96,
      97,
      98,
      99,
      100,
      101,
      102,
      103,
      104,
      105,
      106,
      107,
      108,
      109,
      110,
      111,
      112,
      113,
      114,
      115,
      116,
      117,
      118,
      119,
      120
    ]

-- | Length prefix table: (base_length, extra_bits)
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
                  base = 2 + ((code + 2) .&. complement 1) `shiftL` extraBits
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
  let totalPixels = width * height
  output <- VSM.new totalPixels

  let loop !pos !cache !r
        | pos >= totalPixels = do
            result <- VS.unsafeFreeze output
            return $ Right (result, r)
        | otherwise = do
            let (x, y) = pos `divMod` width
                groupIdx = getEntropyGroup x y maybeEntropyImage width

            let (greenSym, r1) = decodeSymbol (pcgGreen codeGroup) r

            if greenSym < 256
              then do
                let (redSym, r2) = decodeSymbol (pcgRed codeGroup) r1
                    (blueSym, r3) = decodeSymbol (pcgBlue codeGroup) r2
                    (alphaSym, r4) = decodeSymbol (pcgAlpha codeGroup) r3

                    color = packColor (fromIntegral alphaSym) (fromIntegral redSym) (fromIntegral greenSym) (fromIntegral blueSym)

                VSM.write output pos color

                let cache' = case maybeCache of
                      Nothing -> cache
                      Just c -> insertColor color c

                loop (pos + 1) cache' r4
              else
                if greenSym < 280
                  then do
                    let lengthCode = fromIntegral greenSym
                        (baseLen, extraBits) = lengthPrefixTable VU.! lengthCode
                        (extra, r2) = readBits extraBits r1
                        len = baseLen + fromIntegral extra

                    let (distSym, r3) = decodeSymbol (pcgDistance codeGroup) r2

                    dist <-
                      if distSym < 120
                        then return $ kDistanceMap VU.! fromIntegral distSym
                        else do
                          let distCode = fromIntegral distSym - 120
                              extraBits2 = distancePrefixTable VU.! distCode
                              (extra2, _) = readBits extraBits2 r3
                              base = if distCode < 4 then distCode + 1 else 5 + ((distCode - 2) .&. complement 1) `shiftL` extraBits2
                          return $ base + fromIntegral extra2 + 1

                    copyLoop pos dist len output cache maybeCache r3
                  else do
                    let cacheIdx = fromIntegral greenSym - 280
                    case maybeCache of
                      Nothing -> return $ Left "Color cache symbol but no cache"
                      Just c -> do
                        let color = lookupColor cacheIdx c
                        VSM.write output pos color
                        let cache' = insertColor color c
                        loop (pos + 1) cache' r1

      copyLoop !pos !dist !len !out !cache !maybeC !r
        | len <= 0 = loop pos cache r
        | otherwise = do
            let srcPos = pos - dist
            when (srcPos < 0) $
              error $
                "Invalid distance: " ++ show dist ++ " at pos " ++ show pos

            color <- VSM.read out srcPos
            VSM.write out pos color

            let cache' = case maybeC of
                  Nothing -> cache
                  Just c -> insertColor color c

            copyLoop (pos + 1) dist (len - 1) out cache' maybeC r

  loop 0 (maybe (createColorCache 0) id maybeCache) reader

-- | Get entropy group index for a pixel
getEntropyGroup :: Int -> Int -> Maybe (VS.Vector Word32, Int) -> Int -> Int
getEntropyGroup _x _y Nothing _width = 0
getEntropyGroup x y (Just (entropyImage, prefixBits)) width =
  let entropyWidth = (width + (1 `shiftL` prefixBits) - 1) `shiftR` prefixBits
      entropyX = x `shiftR` prefixBits
      entropyY = y `shiftR` prefixBits
      entropyIdx = entropyY * entropyWidth + entropyX
      pixel = entropyImage VS.! entropyIdx
      green = (pixel `shiftR` 8) .&. 0xFF
   in fromIntegral green

-- | Pack ARGB components into a Word32
packColor :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
packColor a r g b =
  (fromIntegral a `shiftL` 24)
    .|. (fromIntegral r `shiftL` 16)
    .|. (fromIntegral g `shiftL` 8)
    .|. fromIntegral b
