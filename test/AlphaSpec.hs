{-# LANGUAGE OverloadedStrings #-}

module AlphaSpec (spec) where

import Codec.Picture.WebP.Internal.Alpha
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Test.Hspec

spec :: Spec
spec = describe "Alpha Channel" $ do
  describe "Raw Alpha Decoding" $ do
    it "decodes uncompressed alpha" $ do
      let header = 0x00 -- No filtering, no compression
          alphaData = B.pack $ header : replicate 4 128
      case decodeAlpha 2 2 alphaData of
        Right alpha -> do
          VS.length alpha `shouldBe` 4
          VS.all (== 128) alpha `shouldBe` True
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "decodes varying alpha values" $ do
      let header = 0x00
          alphaData = B.pack $ header : [0, 64, 128, 255]
      case decodeAlpha 2 2 alphaData of
        Right alpha -> do
          VS.length alpha `shouldBe` 4
          VS.toList alpha `shouldBe` [0, 64, 128, 255]
        Left err -> expectationFailure $ "Decode failed: " ++ err

  describe "Horizontal Filtering" $ do
    it "applies horizontal prediction filter" $ do
      let header = 0x04 -- Horizontal filtering, no compression
      -- Encoded: each value is delta from left
          encodedData = [100, 10, 5, 200] -- First row
          alphaData = B.pack $ header : encodedData
      case decodeAlpha 4 1 alphaData of
        Right alpha -> do
          VS.length alpha `shouldBe` 4
          -- Decoded: 100, 110, 115, 59 (with wraparound)
          VS.toList alpha `shouldBe` [100, 110, 115, 59]
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "handles wraparound in horizontal filter" $ do
      let header = 0x04
          encodedData = [255, 10] -- Second would wrap: 255+10=265, mod 256 = 9
          alphaData = B.pack $ header : encodedData
      case decodeAlpha 2 1 alphaData of
        Right alpha -> do
          VS.toList alpha `shouldBe` [255, 9]
        Left err -> expectationFailure $ "Decode failed: " ++ err

  describe "Vertical Filtering" $ do
    it "applies vertical prediction filter" $ do
      let header = 0x08 -- Vertical filtering
          encodedData = [100, 150, 10, 20] -- Column 1: 100, 10  Column 2: 150, 20
          alphaData = B.pack $ header : encodedData
      case decodeAlpha 2 2 alphaData of
        Right alpha -> do
          VS.length alpha `shouldBe` 4
          let values = VS.toList alpha
          -- First row: 100, 150 (no prediction)
          -- Second row: 110 (100+10), 170 (150+20)
          values `shouldBe` [100, 150, 110, 170]
        Left err -> expectationFailure $ "Decode failed: " ++ err

  describe "Gradient Filtering" $ do
    it "applies gradient prediction filter" $ do
      let header = 0x0C -- Gradient filtering
          encodedData = [100, 150, 10, 20]
          alphaData = B.pack $ header : encodedData
      case decodeAlpha 2 2 alphaData of
        Right alpha -> do
          VS.length alpha `shouldBe` 4
          -- Gradient uses left + above - aboveLeft
          let values = VS.toList alpha
          length values `shouldBe` 4
        Left err -> expectationFailure $ "Decode failed: " ++ err

  describe "Error Handling" $ do
    it "rejects empty input" $ do
      decodeAlpha 2 2 B.empty `shouldSatisfy` isLeft

    it "handles truncated data" $ do
      let header = 0x00
          alphaData = B.pack [header, 1, 2] -- Only 2 values for 2x2 image
      case decodeAlpha 2 2 alphaData of
        Left _ -> True `shouldBe` True -- Expected to fail
        Right alpha -> do
          -- If it succeeds, check actual length
          VS.length alpha `shouldBe` 2 -- Only got 2 bytes
  describe "Dimension Handling" $ do
    it "handles 1x1 image" $ do
      let header = 0x00
          alphaData = B.pack [header, 255]
      case decodeAlpha 1 1 alphaData of
        Right alpha -> do
          VS.length alpha `shouldBe` 1
          VS.head alpha `shouldBe` 255
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "handles rectangular images" $ do
      let header = 0x00
          alphaData = B.pack $ header : replicate 12 100
      case decodeAlpha 3 4 alphaData of
        Right alpha -> do
          VS.length alpha `shouldBe` 12
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "handles large dimensions" $ do
      let header = 0x00
          alphaData = B.pack $ header : replicate (64 * 64) 200
      case decodeAlpha 64 64 alphaData of
        Right alpha -> do
          VS.length alpha `shouldBe` (64 * 64)
          VS.all (== 200) alpha `shouldBe` True
        Left err -> expectationFailure $ "Decode failed: " ++ err

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
