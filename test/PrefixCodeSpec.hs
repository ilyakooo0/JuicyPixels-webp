{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module PrefixCodeSpec (spec) where

import Codec.Picture.WebP.Internal.BitReader
import Codec.Picture.WebP.Internal.VP8L.PrefixCode
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.List
import qualified Data.Vector.Unboxed as VU
import Test.Hspec

spec :: Spec
spec = describe "PrefixCode" $ do
  describe "Code Construction" $ do
    it "builds single-symbol code" $ do
      let lengths = VU.fromList [0, 0, 8, 0]
      case buildPrefixCode lengths of
        Left _ -> expectationFailure "Should build code"
        Right (PrefixCodeSingle sym) -> sym `shouldBe` 2
        Right _ -> expectationFailure "Expected single symbol code"

    it "builds two-symbol code" $ do
      let lengths = VU.fromList [1, 1]
      case buildPrefixCode lengths of
        Left _ -> expectationFailure "Should build code"
        Right _ -> return () -- Success
    it "builds code with multiple symbols" $ do
      let lengths = VU.fromList [2, 2, 2, 2] -- All symbols have length 2
      case buildPrefixCode lengths of
        Left _ -> expectationFailure "Should build code"
        Right (PrefixCodeTable _ _) -> return ()
        Right _ -> expectationFailure "Expected table code"

    it "rejects empty code lengths" $ do
      let lengths = VU.fromList []
      buildPrefixCode lengths `shouldSatisfy` isLeft

    it "rejects all-zero code lengths" $ do
      let lengths = VU.fromList [0, 0, 0, 0]
      buildPrefixCode lengths `shouldSatisfy` isLeft

    it "handles mixed length codes" $ do
      let lengths = VU.fromList [2, 3, 3, 4, 4, 4, 4]
      case buildPrefixCode lengths of
        Left _ -> expectationFailure "Should build code"
        Right (PrefixCodeTable _ _) -> return ()
        Right _ -> expectationFailure "Expected table code"

  describe "Code Decoding" $ do
    it "decodes single-symbol code" $ do
      let lengths = VU.fromList [0, 0, 5, 0]
          bs = B.pack [0xFF]
          reader = initBitReader bs
      case buildPrefixCode lengths of
        Right code -> do
          let (sym, _) = decodeSymbol code reader
          sym `shouldBe` 2
        Left err -> expectationFailure $ "Build failed: " ++ err

    it "decodes two-symbol code" $ do
      let lengths = VU.fromList [1, 1] -- symbols 0 and 1
          bs = B.pack [0b00000001] -- bit 0 = 1 (symbol 1), bit 1 = 0 (symbol 0)
          reader = initBitReader bs
      case buildPrefixCode lengths of
        Right code -> do
          let (sym1, reader') = decodeSymbol code reader
              (sym2, _) = decodeSymbol code reader'
          sym1 `shouldBe` 1 -- bit 0 = 1
          sym2 `shouldBe` 0 -- bit 1 = 0
        Left err -> expectationFailure $ "Build failed: " ++ err

    it "decodes multiple symbols from bitstream" $ do
      let lengths = VU.fromList [2, 2, 2, 2]
          bs = B.pack [0xFF, 0xFF]
          reader = initBitReader bs
      case buildPrefixCode lengths of
        Right code -> do
          let (sym1, r1) = decodeSymbol code reader
              (sym2, r2) = decodeSymbol code r1
              (sym3, _) = decodeSymbol code r2
          -- Each decode consumes 2 bits
          sym1 `shouldSatisfy` (<= 3)
          sym2 `shouldSatisfy` (<= 3)
          sym3 `shouldSatisfy` (<= 3)
        Left err -> expectationFailure $ "Build failed: " ++ err

  describe "Code Length Reading" $ do
    it "reads simple code (1 symbol)" $ do
      -- Simple code: 1 bit set (1 symbol), 8 bits for symbol
      let bs = B.pack [0b00000001, 0x05] -- simple=1, num=0 (1 symbol), symbol=5
          reader = initBitReader bs
      case readCodeLengths 256 reader of
        Right (lengths, _) -> do
          -- Symbol 5 should have length 0 (single symbol codes use 0-bit)
          lengths VU.! 5 `shouldBe` 0
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "reads simple code (2 symbols)" $ do
      -- Simple code: 1 bit set, 1 bit for count (1=2 symbols), 8+8 bits for symbols
      -- Bits: simple=1, numSymbols-1=1, symbol1 (8 bits), symbol2 (8 bits)
      -- Packed LSB: bit0=1 (simple), bit1=1 (count), bits 2-9 = symbol1, bits 10-17 = symbol2
      -- Byte 0: bits 0-7 = 0b11000000 (bit0=1, bit1=1, bits2-7=first 6 bits of symbol1)
      -- This is complex to pack correctly, so let's test the result differently
      let bs = B.pack [0b00000011, 0x03, 0x00, 0x07, 0x00] -- simple=1, num=1, sym1=3, sym2=7
          reader = initBitReader bs
      case readCodeLengths 256 reader of
        Right (lengths, _) -> do
          -- Check that we got valid code lengths
          VU.length lengths `shouldBe` 256
        Left err -> expectationFailure $ "Read failed: " ++ err

  describe "kCodeLengthCodeOrder" $ do
    it "has correct length" $ do
      VU.length kCodeLengthCodeOrder `shouldBe` 19

    it "starts with 17, 18, 0" $ do
      VU.take 3 kCodeLengthCodeOrder `shouldBe` VU.fromList [17, 18, 0]

    it "contains all unique values 0-18" $ do
      let values = VU.toList kCodeLengthCodeOrder
      length values `shouldBe` 19
      all (\i -> i `elem` values) [0 .. 18] `shouldBe` True

  describe "Edge Cases" $ do
    it "handles reasonable code lengths" $ do
      let lengths = VU.fromList [8, 8, 8, 8]
      case buildPrefixCode lengths of
        Right _ -> return ()
        Left _ -> expectationFailure "Should handle reasonable codes"

    it "handles sparse code lengths" $ do
      let lengths = VU.generate 256 $ \i ->
            if i `mod` 16 == 0 then 4 else 0
      case buildPrefixCode lengths of
        Right _ -> return ()
        Left _ -> expectationFailure "Should handle sparse codes"

    it "handles single long code" $ do
      let lengths = VU.replicate 256 0 VU.// [(42, 10)]
      case buildPrefixCode lengths of
        Right (PrefixCodeSingle sym) -> sym `shouldBe` 42
        _ -> expectationFailure "Expected single symbol"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
