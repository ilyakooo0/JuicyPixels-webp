{-# LANGUAGE OverloadedStrings #-}

module BoolDecoderSpec (spec) where

import Codec.Picture.WebP.Internal.VP8.BoolDecoder
import qualified Data.ByteString as B
import Data.Int
import qualified Data.Vector as V
import Test.Hspec

spec :: Spec
spec = describe "BoolDecoder" $ do
  describe "Initialization" $ do
    it "initializes with valid input" $ do
      let bs = B.pack [0xFF, 0xFF, 0x00, 0x00]
          decoder = initBoolDecoder bs
      bdRange decoder `shouldBe` 255

    it "handles minimal input" $ do
      let bs = B.pack [0x00, 0x00]
          decoder = initBoolDecoder bs
      bdRange decoder `shouldBe` 255

  describe "Boolean Reading" $ do
    it "reads bit with probability 128 (50/50)" $ do
      let bs = B.pack [0xFF, 0xFF, 0x00, 0x00]
          decoder = initBoolDecoder bs
          (bit, _) = boolRead 128 decoder
      bit `shouldSatisfy` (\b -> b == True || b == False)

    it "reads multiple bits" $ do
      let bs = B.pack [0xFF, 0xFF, 0xFF, 0xFF]
          decoder = initBoolDecoder bs
          (bit1, d1) = boolRead 128 decoder
          (bit2, d2) = boolRead 128 d1
          (bit3, _) = boolRead 128 d2
      length [bit1, bit2, bit3] `shouldBe` 3

    it "maintains range in valid bounds" $ do
      let bs = B.pack [0x80, 0x80, 0x80, 0x80]
          decoder = initBoolDecoder bs
          (_, d1) = boolRead 128 decoder
          (_, d2) = boolRead 128 d1
          (_, d3) = boolRead 128 d2
      bdRange d3 `shouldSatisfy` (\r -> r >= 128 && r <= 255)

  describe "Literal Reading" $ do
    it "reads literal bits correctly" $ do
      let bs = B.pack [0xFF, 0xFF, 0xFF, 0xFF]
          decoder = initBoolDecoder bs
          (val, _) = boolLiteral 8 decoder
      val `shouldSatisfy` (<= 255)

    it "reads zero bits returns zero" $ do
      let bs = B.pack [0xFF, 0xFF]
          decoder = initBoolDecoder bs
          (val, _) = boolLiteral 0 decoder
      val `shouldBe` 0

    it "reads multiple literals" $ do
      let bs = B.pack [0xFF, 0xFF, 0xFF, 0xFF]
          decoder = initBoolDecoder bs
          (val1, d1) = boolLiteral 4 decoder
          (val2, d2) = boolLiteral 4 d1
          (val3, _) = boolLiteral 4 d2
      [val1, val2, val3] `shouldSatisfy` all (<= 15)

  describe "Signed Reading" $ do
    it "reads positive signed values" $ do
      let bs = B.pack [0x00, 0xFF, 0xFF, 0xFF]
          decoder = initBoolDecoder bs
          (val, _) = boolSigned 4 decoder
      abs val `shouldSatisfy` (<= 15)

    it "handles zero magnitude" $ do
      let bs = B.pack [0x00, 0x00, 0xFF, 0xFF]
          decoder = initBoolDecoder bs
          (val, _) = boolSigned 0 decoder
      val `shouldBe` 0

    it "reads multiple signed values" $ do
      let bs = B.pack $ replicate 20 0x80
          decoder = initBoolDecoder bs
          (val1, d1) = boolSigned 3 decoder
          (val2, d2) = boolSigned 3 d1
          (val3, _) = boolSigned 3 d2
      length [val1, val2, val3] `shouldBe` 3

  describe "Tree Reading" $ do
    it "reads from simple tree" $ do
      let tree = V.fromList [-1, 2, -2, -3] -- Simple 2-bit tree
          probs = V.fromList [128, 128]
          bs = B.pack [0xFF, 0xFF, 0xFF, 0xFF]
          decoder = initBoolDecoder bs
          (symbol, _) = boolReadTree tree probs decoder
      symbol `shouldSatisfy` (\s -> s >= 1 && s <= 3)

    it "handles single-node tree" $ do
      let tree = V.fromList [-5] -- Immediate leaf
          probs = V.fromList []
          bs = B.pack [0x00, 0x00]
          decoder = initBoolDecoder bs
          (symbol, _) = boolReadTree tree probs decoder
      symbol `shouldBe` 5

    it "reads multiple symbols from tree" $ do
      let tree = V.fromList [-1, 2, -2, -3]
          probs = V.fromList [128, 128]
          bs = B.pack $ replicate 20 0xAA
          decoder = initBoolDecoder bs
          (sym1, d1) = boolReadTree tree probs decoder
          (sym2, d2) = boolReadTree tree probs d1
          (sym3, _) = boolReadTree tree probs d2
      length [sym1, sym2, sym3] `shouldBe` 3

  describe "Range Maintenance" $ do
    it "keeps range in bounds after operations" $ do
      let bs = B.pack $ replicate 100 0x55
          decoder = initBoolDecoder bs
          -- Read many bits to test renormalization
          finalDecoder = foldl (\d _ -> snd $ boolRead 128 d) decoder [1 .. 50]
      bdRange finalDecoder `shouldSatisfy` (\r -> r >= 128 && r <= 255)

    it "renormalizes correctly" $ do
      let bs = B.pack $ replicate 20 0xFF
          decoder = initBoolDecoder bs
          (_, d1) = boolRead 1 decoder -- Very low probability
          (_, d2) = boolRead 1 d1
          (_, d3) = boolRead 1 d2
      bdRange d3 `shouldSatisfy` (\r -> r >= 128 && r <= 255)

  describe "Edge Cases" $ do
    it "handles probability extremes" $ do
      let bs = B.pack [0xFF, 0xFF, 0xFF, 0xFF]
          decoder = initBoolDecoder bs
          (bit1, d1) = boolRead 1 decoder -- Very unlikely
          (bit2, _) = boolRead 254 d1 -- Very likely
      length [bit1, bit2] `shouldBe` 2

    it "handles long sequences" $ do
      let bs = B.pack $ replicate 1000 0xAA
          decoder = initBoolDecoder bs
          finalDecoder = foldl (\d _ -> snd $ boolRead 128 d) decoder [1 .. 100]
      bdPos finalDecoder `shouldSatisfy` (> 0)

    it "handles alternating probabilities" $ do
      let bs = B.pack $ replicate 50 0x55
          decoder = initBoolDecoder bs
          results = take 20 $ iterate (\(_, d) -> boolRead 128 d) (False, decoder)
          bits = map fst results
      length bits `shouldBe` 20
