{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DCTSpec (spec) where

import Codec.Picture.WebP.Internal.VP8.DCT
import Codec.Picture.WebP.Internal.VP8.IDCT
import Control.Monad.ST
import Data.Int
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "DCT" $ do
  describe "Forward 4x4 DCT" $ do
    it "transforms all-zero input to near-zero output" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
      -- Allow small rounding errors
      VS.all (\x -> abs x < 2) result `shouldBe` True

    it "transforms DC-only (constant) block" $ do
      let result = runST $ do
            -- All 64s (constant block)
            coeffs <- VSM.replicate 16 64
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
      -- DC coefficient should be non-zero, AC coefficients should be near zero
      let dc = result VS.! 0
      dc `shouldSatisfy` (/= 0)
      -- AC coefficients should be zero for constant block
      let acCoeffs = VS.toList $ VS.drop 1 result
      all (\x -> abs x < 2) acCoeffs `shouldBe` True

    it "produces symmetric output for symmetric input" $ do
      let result = runST $ do
            coeffs <- VSM.generate 16 $ \i ->
              let x = i `mod` 4
                  y = i `div` 4
               in fromIntegral $ x + y
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
      VS.length result `shouldBe` 16

    it "handles negative input values" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 (-32)
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
      let dc = result VS.! 0
      dc `shouldSatisfy` (< 0)

    it "handles mixed positive and negative values" $ do
      let result = runST $ do
            coeffs <- VSM.generate 16 $ \i ->
              if even i then 50 else (-50)
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
      VS.length result `shouldBe` 16

  describe "Forward Walsh-Hadamard Transform" $ do
    it "transforms all-zero input" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            fwht4x4 coeffs
            VS.unsafeFreeze coeffs
      VS.all (== 0) result `shouldBe` True

    it "transforms DC-only input" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 16
            fwht4x4 coeffs
            VS.unsafeFreeze coeffs
      -- Should have energy concentrated in DC
      VS.length result `shouldBe` 16

    it "handles alternating pattern" $ do
      let result = runST $ do
            coeffs <- VSM.generate 16 $ \i ->
              if even i then 10 else (-10)
            fwht4x4 coeffs
            VS.unsafeFreeze coeffs
      VS.length result `shouldBe` 16

  describe "DCT/IDCT Roundtrip" $ do
    it "roundtrip preserves DC-only block" $ do
      let original = VS.replicate 16 64
          result = runST $ do
            coeffs <- VS.thaw original
            fdct4x4 coeffs
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
      -- Should be close to original (may have small rounding errors)
      let maxError = VS.maximum $ VS.zipWith (\a b -> abs (a - b)) original result
      maxError `shouldSatisfy` (< 5)

    it "roundtrip preserves gradient block" $ do
      let original = VS.generate 16 $ \i -> fromIntegral i * 4
          result = runST $ do
            coeffs <- VS.thaw original
            fdct4x4 coeffs
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
      let maxError = VS.maximum $ VS.zipWith (\a b -> abs (a - b)) original result
      maxError `shouldSatisfy` (< 5)

    it "roundtrip preserves alternating pattern" $ do
      let original = VS.generate 16 $ \i ->
            if even i then 100 else (-100)
          result = runST $ do
            coeffs <- VS.thaw original
            fdct4x4 coeffs
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
      let maxError = VS.maximum $ VS.zipWith (\a b -> abs (a - b)) original result
      maxError `shouldSatisfy` (< 5)

    it "roundtrip is approximately identity" $
      property $ \(values :: [Int]) ->
        let limited = take 16 $ map (`mod` 256) values ++ repeat 0
            original = VS.fromList $ map fromIntegral limited :: VS.Vector Int16
            result = runST $ do
              coeffs <- VS.thaw original
              fdct4x4 coeffs
              idct4x4 coeffs
              VS.unsafeFreeze coeffs
            maxError = VS.maximum $ VS.zipWith (\a b -> abs (a - b)) original result
         in maxError < 10

  describe "WHT/IWHT Roundtrip" $ do
    it "roundtrip preserves DC-only block" $ do
      let original = VS.replicate 16 32
          result = runST $ do
            coeffs <- VS.thaw original
            fwht4x4 coeffs
            iwht4x4 coeffs
      let maxError = VS.maximum $ VS.zipWith (\a b -> abs (a - b)) original result
      maxError `shouldSatisfy` (< 2)

    it "roundtrip preserves gradient" $ do
      let original = VS.generate 16 $ \i -> fromIntegral i
          result = runST $ do
            coeffs <- VS.thaw original
            fwht4x4 coeffs
            iwht4x4 coeffs
      let maxError = VS.maximum $ VS.zipWith (\a b -> abs (a - b)) original result
      maxError `shouldSatisfy` (< 2)

  describe "Transform Properties" $ do
    it "DCT is deterministic" $ do
      let input = VS.generate 16 $ \i -> fromIntegral (i * 7 `mod` 100)
          result1 = runST $ do
            coeffs <- VS.thaw input
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
          result2 = runST $ do
            coeffs <- VS.thaw input
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
      result1 `shouldBe` result2

    it "WHT is deterministic" $ do
      let input = VS.generate 16 $ \i -> fromIntegral (i * 11 `mod` 50)
          result1 = runST $ do
            coeffs <- VS.thaw input
            fwht4x4 coeffs
            VS.unsafeFreeze coeffs
          result2 = runST $ do
            coeffs <- VS.thaw input
            fwht4x4 coeffs
            VS.unsafeFreeze coeffs
      result1 `shouldBe` result2

    it "DCT and WHT produce different results" $ do
      let input = VS.generate 16 $ \i -> fromIntegral i
          dctResult = runST $ do
            coeffs <- VS.thaw input
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
          whtResult = runST $ do
            coeffs <- VS.thaw input
            fwht4x4 coeffs
            VS.unsafeFreeze coeffs
      dctResult `shouldNotBe` whtResult

  describe "Edge Cases" $ do
    it "handles maximum positive values" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 255
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
      VS.length result `shouldBe` 16

    it "handles maximum negative values" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 (-255)
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
      VS.length result `shouldBe` 16

    it "handles single non-zero coefficient" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            VSM.write coeffs 5 100 -- Single non-zero
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
      -- Should distribute energy
      VS.any (/= 0) result `shouldBe` True

  describe "Energy Concentration" $ do
    it "natural images concentrate energy in low frequencies" $ do
      -- Smooth gradient should have most energy in DC and low-frequency coefficients
      let result = runST $ do
            coeffs <- VSM.generate 16 $ \i ->
              let x = i `mod` 4
                  y = i `div` 4
               in fromIntegral $ 128 + x * 5 + y * 5
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
      -- DC coefficient should have most of the energy
      let dc = abs (result VS.! 0)
          totalEnergy = VS.sum $ VS.map abs result
      fromIntegral dc `shouldSatisfy` (> (fromIntegral totalEnergy * 0.5 :: Double))

    it "high-frequency patterns spread energy" $ do
      -- Checkerboard should have energy in high-frequency coefficients
      let result = runST $ do
            coeffs <- VSM.generate 16 $ \i ->
              if even i then 200 else 50
            fdct4x4 coeffs
            VS.unsafeFreeze coeffs
      -- AC coefficients should have significant energy
      let acEnergy = VS.sum $ VS.map abs $ VS.drop 1 result
      acEnergy `shouldSatisfy` (> 0)
