{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified AlphaSpec
import qualified AnimationSpec
import qualified BitReaderSpec
import qualified BitWriterSpec
import qualified BoolDecoderSpec
import qualified ColorConvertSpec
import qualified ContainerSpec
import qualified DCTSpec
import qualified EdgeCasesSpec
import qualified GoldenFilesSpec
import qualified GoldenSpec
import qualified IDCTSpec
import qualified ImageDecodingSpec
import qualified LoopFilterSpec
import qualified LZ77Spec
import qualified PrefixCodeSpec
import qualified PropertyExtendedSpec
import qualified PropertySpec
import qualified QualitySpec
import qualified RealFilesSpec
import qualified RealImageSpec
import qualified RoundtripSpec
import qualified SpecComplianceSpec
import qualified StressSpec
import Test.Hspec
import qualified TransformSpec
import qualified VP8EncodeSpec

main :: IO ()
main = hspec $ do
  -- Low-level component tests
  describe "BitReader" BitReaderSpec.spec
  describe "BitWriter" BitWriterSpec.spec
  describe "PrefixCode" PrefixCodeSpec.spec
  describe "Container" ContainerSpec.spec
  describe "LZ77" LZ77Spec.spec

  -- VP8 (lossy) component tests
  describe "BoolDecoder" BoolDecoderSpec.spec
  describe "DCT" DCTSpec.spec
  describe "IDCT" IDCTSpec.spec
  describe "LoopFilter" LoopFilterSpec.spec
  describe "ColorConvert" ColorConvertSpec.spec

  -- VP8L (lossless) component tests
  describe "VP8L Transforms" TransformSpec.spec

  -- Decoding tests
  describe "Image Decoding" ImageDecodingSpec.spec
  describe "Alpha Channel" AlphaSpec.spec
  describe "Real Images" RealImageSpec.spec
  describe "Real Files" RealFilesSpec.spec

  -- Encoding tests
  describe "VP8 Lossy Encoder" VP8EncodeSpec.spec

  -- Roundtrip and integration tests
  describe "Roundtrip Tests" RoundtripSpec.spec
  describe "Animation Tests" AnimationSpec.spec
  describe "Golden Tests" GoldenSpec.spec
  describe "Golden File Tests" GoldenFilesSpec.spec

  -- Edge cases and stress tests
  describe "Edge Cases" EdgeCasesSpec.spec
  describe "Stress Tests" StressSpec.spec
  describe "Quality Tests" QualitySpec.spec

  -- Property-based tests
  describe "Property-Based Tests" PropertySpec.spec
  describe "Extended Property Tests" PropertyExtendedSpec.spec

  -- Spec compliance tests
  describe "Spec Compliance" SpecComplianceSpec.spec
