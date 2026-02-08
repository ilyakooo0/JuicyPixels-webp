{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified AlphaSpec
import qualified BitReaderSpec
import qualified BoolDecoderSpec
import qualified ContainerSpec
import qualified EdgeCasesSpec
import qualified GoldenFilesSpec
import qualified GoldenSpec
import qualified IDCTSpec
import qualified ImageDecodingSpec
import qualified PrefixCodeSpec
import qualified PropertySpec
import qualified QualitySpec
import qualified RealFilesSpec
import qualified RealImageSpec
import qualified RoundtripSpec
import Test.Hspec
import qualified TransformSpec
import qualified VP8EncodeSpec

main :: IO ()
main = hspec $ do
  describe "BitReader" BitReaderSpec.spec
  describe "PrefixCode" PrefixCodeSpec.spec
  describe "Container" ContainerSpec.spec
  describe "VP8L Transforms" TransformSpec.spec
  describe "Image Decoding" ImageDecodingSpec.spec
  describe "Alpha Channel" AlphaSpec.spec
  describe "BoolDecoder" BoolDecoderSpec.spec
  describe "IDCT" IDCTSpec.spec
  describe "Real Images" RealImageSpec.spec
  describe "Real Files" RealFilesSpec.spec
  describe "VP8 Lossy Encoder" VP8EncodeSpec.spec
  describe "Roundtrip Tests" RoundtripSpec.spec
  describe "Golden Tests" GoldenSpec.spec
  describe "Golden File Tests" GoldenFilesSpec.spec
  describe "Edge Cases" EdgeCasesSpec.spec
  describe "Quality Tests" QualitySpec.spec
  describe "Property-Based Tests" PropertySpec.spec
