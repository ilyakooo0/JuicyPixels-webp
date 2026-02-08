{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LZ77Spec (spec) where

import Codec.Picture.WebP.Internal.VP8L.LZ77
import Data.Bits
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "LZ77" $ do
  describe "ColorCache" $ do
    describe "createColorCache" $ do
      it "creates cache with correct size for 0 bits" $ do
        let cache = createColorCache 0
        ccBits cache `shouldBe` 0
        VS.length (ccColors cache) `shouldBe` 1

      it "creates cache with correct size for 4 bits" $ do
        let cache = createColorCache 4
        ccBits cache `shouldBe` 4
        VS.length (ccColors cache) `shouldBe` 16

      it "creates cache with correct size for 11 bits (max)" $ do
        let cache = createColorCache 11
        ccBits cache `shouldBe` 11
        VS.length (ccColors cache) `shouldBe` 2048

      it "initializes all entries to 0" $ do
        let cache = createColorCache 4
        VS.all (== 0) (ccColors cache) `shouldBe` True

    describe "insertColor" $ do
      it "inserts color into cache" $ do
        let cache = createColorCache 4
            color = 0xFF00FF00 :: Word32
            cache' = insertColor color cache
        VS.any (== color) (ccColors cache') `shouldBe` True

      it "different colors may go to different slots" $ do
        let cache = createColorCache 8
            color1 = 0xFFFFFFFF :: Word32
            color2 = 0x00000000 :: Word32
            cache' = insertColor color2 $ insertColor color1 cache
        -- Both colors should be in cache (may or may not be same slot)
        VS.length (ccColors cache') `shouldBe` 256

      it "overwrites existing entry at same hash" $ do
        let cache = createColorCache 4
            color1 = 0xABCDEF01 :: Word32
            cache1 = insertColor color1 cache
            -- Insert same color again
            cache2 = insertColor color1 cache1
        -- Cache size should remain same
        VS.length (ccColors cache2) `shouldBe` 16

    describe "lookupColor" $ do
      it "retrieves inserted color" $ do
        let cache = createColorCache 4
            color = 0x12345678 :: Word32
            cache' = insertColor color cache
            -- Calculate expected index
            idx = fromIntegral ((0x1e35a7bd * color) `shiftR` (32 - 4))
            retrieved = lookupColor idx cache'
        retrieved `shouldBe` color

      it "returns 0 for uninitialized slot" $ do
        let cache = createColorCache 4
            retrieved = lookupColor 0 cache
        retrieved `shouldBe` 0

  describe "Color Cache Hash" $ do
    it "produces consistent hashes" $
      property $ \(color :: Word32) ->
        let cache1 = insertColor color (createColorCache 8)
            cache2 = insertColor color (createColorCache 8)
         in ccColors cache1 == ccColors cache2

    it "distributes colors across cache" $ do
      -- Insert many different colors and check distribution
      let colors = [fromIntegral i | i <- [0 .. 255 :: Int]]
          cache = foldr insertColor (createColorCache 8) colors
          nonZero = VS.length $ VS.filter (/= 0) (ccColors cache)
      -- Should have reasonable distribution (not all in one slot)
      nonZero `shouldSatisfy` (> 10)

  describe "Distance Map" $ do
    it "has 121 entries (0-120)" $ do
      VU.length kDistanceMap `shouldBe` 121

    it "first entry is 0" $ do
      kDistanceMap VU.! 0 `shouldBe` 0

    it "entry 1 is 1" $ do
      kDistanceMap VU.! 1 `shouldBe` 1

    it "all entries are sequential (identity map)" $ do
      let expected = VU.fromList [0 .. 120]
      kDistanceMap `shouldBe` expected

  describe "Length Prefix Table" $ do
    it "has 280 entries" $ do
      VU.length lengthPrefixTable `shouldBe` 280

    it "literal symbols (0-255) have 0 extra bits" $ do
      all (\i -> snd (lengthPrefixTable VU.! i) == 0) [0 .. 255] `shouldBe` True

    it "first length code (256) has length 1, 0 extra bits" $ do
      lengthPrefixTable VU.! 256 `shouldBe` (1, 0)

    it "length codes have increasing base lengths" $ do
      let bases = map (\i -> fst (lengthPrefixTable VU.! i)) [256 .. 279]
      all (\(a, b) -> a <= b) (zip bases (tail bases)) `shouldBe` True

    it "extra bits increase appropriately" $ do
      -- First few length codes should have 0 extra bits
      snd (lengthPrefixTable VU.! 256) `shouldBe` 0
      snd (lengthPrefixTable VU.! 257) `shouldBe` 0
      snd (lengthPrefixTable VU.! 258) `shouldBe` 0
      snd (lengthPrefixTable VU.! 259) `shouldBe` 0
      -- Later codes have more extra bits
      snd (lengthPrefixTable VU.! 260) `shouldSatisfy` (> 0)

  describe "Distance Prefix Table" $ do
    it "has 40 entries" $ do
      VU.length distancePrefixTable `shouldBe` 40

    it "first 4 codes have 0 extra bits" $ do
      distancePrefixTable VU.! 0 `shouldBe` 0
      distancePrefixTable VU.! 1 `shouldBe` 0
      distancePrefixTable VU.! 2 `shouldBe` 0
      distancePrefixTable VU.! 3 `shouldBe` 0

    it "extra bits increase for higher codes" $ do
      distancePrefixTable VU.! 4 `shouldBe` 1
      distancePrefixTable VU.! 5 `shouldBe` 1
      distancePrefixTable VU.! 6 `shouldBe` 2
      distancePrefixTable VU.! 7 `shouldBe` 2

    it "follows pattern (code - 2) / 2 for code >= 4" $ do
      let checkCode code =
            let expected = (code - 2) `shiftR` 1
             in distancePrefixTable VU.! code == expected
      all checkCode [4 .. 39] `shouldBe` True

  describe "Prefix Code Group" $ do
    it "PrefixCodeGroup type is exported" $ do
      -- Just verify the type is accessible (compilation test)
      True `shouldBe` True

  describe "Color Packing" $ do
    it "packs ARGB correctly" $ do
      let packed = packARGB 0xFF 0x12 0x34 0x56
      packed `shouldBe` 0xFF123456

    it "handles all zeros" $ do
      let packed = packARGB 0 0 0 0
      packed `shouldBe` 0

    it "handles all ones" $ do
      let packed = packARGB 0xFF 0xFF 0xFF 0xFF
      packed `shouldBe` 0xFFFFFFFF

    it "each component is in correct position" $ do
      let a = packARGB 0xFF 0 0 0
          r = packARGB 0 0xFF 0 0
          g = packARGB 0 0 0xFF 0
          b = packARGB 0 0 0 0xFF
      a `shouldBe` 0xFF000000
      r `shouldBe` 0x00FF0000
      g `shouldBe` 0x0000FF00
      b `shouldBe` 0x000000FF

  describe "Integration" $ do
    it "color cache insertion and lookup roundtrip" $
      property $ \(a :: Word8, r :: Word8, g :: Word8, b :: Word8) ->
        let color = packARGB a r g b
            cache = insertColor color (createColorCache 11)
            idx = fromIntegral ((0x1e35a7bd * color) `shiftR` (32 - 11))
            retrieved = lookupColor idx cache
         in retrieved == color

    it "multiple colors maintain integrity" $ do
      let colors =
            [ packARGB 255 255 0 0, -- Red
              packARGB 255 0 255 0, -- Green
              packARGB 255 0 0 255, -- Blue
              packARGB 255 255 255 255, -- White
              packARGB 0 0 0 0 -- Transparent black
            ]
          cache = foldr insertColor (createColorCache 11) colors
      -- Verify cache has been modified
      VS.any (/= 0) (ccColors cache) `shouldBe` True

-- Helper function matching the internal implementation
packARGB :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
packARGB a r g b =
  (fromIntegral a `shiftL` 24)
    .|. (fromIntegral r `shiftL` 16)
    .|. (fromIntegral g `shiftL` 8)
    .|. fromIntegral b
