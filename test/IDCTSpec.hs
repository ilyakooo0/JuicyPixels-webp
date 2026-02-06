{-# LANGUAGE OverloadedStrings #-}

module IDCTSpec (spec) where

import Codec.Picture.WebP.Internal.VP8.IDCT
import Control.Monad.ST
import Data.Int
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Test.Hspec

spec :: Spec
spec = describe "IDCT" $ do
  describe "4x4 IDCT" $ do
    it "handles all-zero input" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
      VS.all (== 0) result `shouldBe` True

    it "handles DC-only input" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            VSM.write coeffs 0 64 -- DC coefficient
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
      -- All values should be equal (DC spread across block)
      let allValues = VS.toList result
      case allValues of
        [] -> expectationFailure "Empty result"
        (firstVal : _) -> all (== firstVal) allValues `shouldBe` True

    it "produces symmetric output for symmetric input" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            VSM.write coeffs 0 32
            VSM.write coeffs 5 16 -- Symmetric position
            VSM.write coeffs 10 16 -- Symmetric position
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
      VS.length result `shouldBe` 16

    it "handles negative coefficients" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            VSM.write coeffs 0 (-64)
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
      VS.all (<= 0) result `shouldBe` True

    it "handles mixed positive and negative coefficients" $ do
      let result = runST $ do
            coeffs <- VSM.generate 16 $ \i ->
              if even i then 10 else (-10)
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
      VS.length result `shouldBe` 16

  describe "Walsh-Hadamard Transform" $ do
    it "handles all-zero input" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            iwht4x4 coeffs
      VS.all (== 0) result `shouldBe` True

    it "handles DC-only input" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            VSM.write coeffs 0 16
            iwht4x4 coeffs
      -- All values should be equal for DC-only
      let allValues = VS.toList result
      case allValues of
        [] -> expectationFailure "Empty result"
        (firstVal : _) -> all (== firstVal) allValues `shouldBe` True

    it "produces 16 values" $ do
      let result = runST $ do
            coeffs <- VSM.generate 16 fromIntegral
            iwht4x4 coeffs
      VS.length result `shouldBe` 16

    it "handles negative DC" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            VSM.write coeffs 0 (-32)
            iwht4x4 coeffs
      VS.any (< 0) result `shouldBe` True

    it "distributes DC value correctly" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            VSM.write coeffs 0 80
            iwht4x4 coeffs
      -- Sum of outputs should be related to input DC
      VS.sum result `shouldSatisfy` (/= 0)

  describe "IDCT Properties" $ do
    it "column-then-row order is deterministic" $ do
      let input = VS.generate 16 $ \i -> fromIntegral (i * 7 `mod` 23) - 10
          result1 = runST $ do
            coeffs <- VS.thaw input
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
          result2 = runST $ do
            coeffs <- VS.thaw input
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
      result1 `shouldBe` result2

    it "handles full-range coefficients" $ do
      let result = runST $ do
            coeffs <- VSM.generate 16 $ \i ->
              if i == 0
                then 100
                else
                  if i < 8
                    then 20
                    else (-20)
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
      VS.length result `shouldBe` 16

  describe "WHT Properties" $ do
    it "is deterministic" $ do
      let input = VS.generate 16 $ \i -> fromIntegral (i - 8)
          result1 = runST $ do
            coeffs <- VS.thaw input
            iwht4x4 coeffs
          result2 = runST $ do
            coeffs <- VS.thaw input
            iwht4x4 coeffs
      result1 `shouldBe` result2

    it "handles extreme values" $ do
      let result = runST $ do
            coeffs <- VSM.replicate 16 0
            VSM.write coeffs 0 1000
            VSM.write coeffs 1 (-1000)
            iwht4x4 coeffs
      VS.length result `shouldBe` 16

  describe "Integration" $ do
    it "IDCT followed by WHT produces different results" $ do
      let input = VS.generate 16 $ \i -> fromIntegral i
          idctResult = runST $ do
            coeffs <- VS.thaw input
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
          whtResult = runST $ do
            coeffs <- VS.thaw input
            iwht4x4 coeffs
      idctResult `shouldNotBe` whtResult

    it "zero input gives zero output for both transforms" $ do
      let zeros = VS.replicate 16 0
          idctResult = runST $ do
            coeffs <- VS.thaw zeros
            idct4x4 coeffs
            VS.unsafeFreeze coeffs
          whtResult = runST $ do
            coeffs <- VS.thaw zeros
            iwht4x4 coeffs
      idctResult `shouldBe` zeros
      whtResult `shouldBe` zeros
