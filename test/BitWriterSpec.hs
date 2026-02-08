{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BitWriterSpec (spec) where

import Codec.Picture.WebP.Internal.BitReader
import Codec.Picture.WebP.Internal.BitWriter
import Data.Bits
import qualified Data.ByteString as B
import Data.Word
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "BitWriter" $ do
  describe "Basic Operations" $ do
    it "creates empty writer" $ do
      let writer = emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 0

    it "writes single bit 1" $ do
      let writer = writeBit True emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 1
      B.index bs 0 `shouldBe` 0x01

    it "writes single bit 0" $ do
      let writer = writeBit False emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 1
      B.index bs 0 `shouldBe` 0x00

    it "writes 8 bits to form a byte" $ do
      -- foldr applies from right to left, so bits go in order: F,T,F,T,F,T,F,T
      -- LSB first packing: bit 0=F, bit 1=T, bit 2=F, bit 3=T, bit 4=F, bit 5=T, bit 6=F, bit 7=T
      -- Binary: 10101010 = 0xAA
      let writer =
            foldr
              (\b w -> writeBit b w)
              emptyBitWriter
              [True, False, True, False, True, False, True, False]
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 1
      B.index bs 0 `shouldBe` 0xAA

  describe "writeBits" $ do
    it "writes 0 bits" $ do
      let writer = writeBits 0 0xFF emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 0

    it "writes 8 bits" $ do
      let writer = writeBits 8 0xAB emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 1
      B.index bs 0 `shouldBe` 0xAB

    it "writes 16 bits" $ do
      let writer = writeBits 16 0x1234 emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 2
      -- LSB first: 0x34, 0x12
      B.index bs 0 `shouldBe` 0x34
      B.index bs 1 `shouldBe` 0x12

    it "writes partial bytes correctly" $ do
      let writer = writeBits 4 0x0F emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 1
      B.index bs 0 `shouldBe` 0x0F

    it "writes multiple partial writes" $ do
      let writer = writeBits 4 0x05 $ writeBits 4 0x0A emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 1
      -- First 4 bits: 0xA (1010), next 4 bits: 0x5 (0101) = 0x5A
      B.index bs 0 `shouldBe` 0x5A

  describe "writeBitsReversed" $ do
    it "reverses 4 bits" $ do
      -- Input: 0b1100 (12), reversed: 0b0011 (3)
      let writer = writeBitsReversed 4 0x0C emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 1
      B.index bs 0 `shouldBe` 0x03

    it "reverses 8 bits" $ do
      -- Input: 0b10110100 (0xB4), reversed: 0b00101101 (0x2D)
      let writer = writeBitsReversed 8 0xB4 emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 1
      B.index bs 0 `shouldBe` 0x2D

    it "reverses 1 bit (unchanged)" $ do
      let writer1 = writeBitsReversed 1 1 emptyBitWriter
          bs1 = bitWriterToByteString $ finalizeBitWriter writer1
      B.index bs1 0 `shouldBe` 0x01

  describe "finalizeBitWriter" $ do
    it "pads to byte boundary" $ do
      let writer = writeBits 3 0x05 emptyBitWriter -- 101
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 1
      B.index bs 0 `shouldBe` 0x05 -- Padded with zeros

    it "is idempotent when already aligned" $ do
      let writer = writeBits 8 0xFF emptyBitWriter
          bs1 = bitWriterToByteString $ finalizeBitWriter writer
          bs2 = bitWriterToByteString $ finalizeBitWriter $ finalizeBitWriter writer
      bs1 `shouldBe` bs2

  describe "Roundtrip with BitReader" $ do
    it "write then read single byte" $ do
      let writer = writeBits 8 0xAB emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
          reader = initBitReader bs
          (val, _) = readBits 8 reader
      val `shouldBe` 0xAB

    it "write then read multiple bytes" $ do
      let writer = writeBits 8 0x34 $ writeBits 8 0x12 emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
          reader = initBitReader bs
          (val1, r1) = readBits 8 reader
          (val2, _) = readBits 8 r1
      val1 `shouldBe` 0x12
      val2 `shouldBe` 0x34

    it "write then read partial bytes" $ do
      let writer = writeBits 5 0x15 $ writeBits 3 0x06 emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
          reader = initBitReader bs
          (val1, r1) = readBits 3 reader
          (val2, _) = readBits 5 r1
      val1 `shouldBe` 0x06
      val2 `shouldBe` 0x15

    it "roundtrip arbitrary values" $
      property $ \(val :: Word8) ->
        let writer = writeBits 8 (fromIntegral val) emptyBitWriter
            bs = bitWriterToByteString $ finalizeBitWriter writer
            reader = initBitReader bs
            (result, _) = readBits 8 reader
         in result == fromIntegral val

    it "roundtrip multiple values" $
      property $ \(v1 :: Word8, v2 :: Word8, v3 :: Word8) ->
        let writer =
              writeBits 8 (fromIntegral v3) $
                writeBits 8 (fromIntegral v2) $
                  writeBits 8 (fromIntegral v1) emptyBitWriter
            bs = bitWriterToByteString $ finalizeBitWriter writer
            reader = initBitReader bs
            (r1, rd1) = readBits 8 reader
            (r2, rd2) = readBits 8 rd1
            (r3, _) = readBits 8 rd2
         in r1 == fromIntegral v1
              && r2 == fromIntegral v2
              && r3 == fromIntegral v3

  describe "Edge Cases" $ do
    it "handles writing 32 bits" $ do
      let writer = writeBits 32 0xDEADBEEF emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 4
      B.index bs 0 `shouldBe` 0xEF
      B.index bs 1 `shouldBe` 0xBE
      B.index bs 2 `shouldBe` 0xAD
      B.index bs 3 `shouldBe` 0xDE

    it "handles many small writes" $ do
      let writer = foldr (writeBits 1 . fromIntegral) emptyBitWriter [1, 0, 1, 0, 1, 0, 1, 0 :: Int]
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 1

    it "handles alternating 1-bit writes" $ do
      let bits = replicate 16 True ++ replicate 16 False
          writer = foldr writeBit emptyBitWriter bits
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 4

    it "handles long sequence of bytes" $ do
      let bytes = [0 .. 255] :: [Word8]
          writer = foldr (writeBits 8 . fromIntegral) emptyBitWriter bytes
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 256

  describe "Bit Order Verification" $ do
    it "LSB-first bit packing" $ do
      -- Write bits in order: 1, 0, 0, 0, 0, 0, 0, 0
      -- Should produce byte 0x01 (LSB first)
      let writer = foldr writeBit emptyBitWriter [False, False, False, False, False, False, False, True]
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.index bs 0 `shouldBe` 0x01

    it "bit accumulation matches spec" $ do
      -- Write 0b11 (3) as 2 bits, then 0b01 (1) as 2 bits
      -- Result: 0b0111 (LSB first accumulation)
      let writer = writeBits 2 0x01 $ writeBits 2 0x03 emptyBitWriter
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.index bs 0 `shouldBe` 0x07

  describe "Integration with Complex Patterns" $ do
    it "simulates Huffman code writing" $ do
      -- Simulate writing variable-length codes
      let writer =
            writeBitsReversed 3 0x05 $ -- 3-bit code
              writeBitsReversed 2 0x02 $ -- 2-bit code
                writeBitsReversed 4 0x0F emptyBitWriter -- 4-bit code
          bs = bitWriterToByteString $ finalizeBitWriter writer
      B.length bs `shouldBe` 2

    it "deterministic output" $
      property $ \(bits :: [Bool]) ->
        let limitedBits = take 64 bits
            writer = foldr writeBit emptyBitWriter limitedBits
            bs1 = bitWriterToByteString $ finalizeBitWriter writer
            bs2 = bitWriterToByteString $ finalizeBitWriter writer
         in bs1 == bs2
