{-# LANGUAGE OverloadedStrings #-}

module BitReaderSpec (spec) where

import Codec.Picture.WebP.Internal.BitReader
import Data.Bits
import qualified Data.ByteString as B
import Data.Word
import Test.Hspec

spec :: Spec
spec = describe "BitReader" $ do
  describe "Basic Operations" $ do
    it "reads bits LSB-first" $ do
      let bs = B.pack [0xCA] -- 11001010
          reader = initBitReader bs
          (bits2, reader') = readBits 2 reader -- bits 0-1: 10
          (bits3, reader'') = readBits 3 reader' -- bits 2-4: 010
          (bits3', _) = readBits 3 reader'' -- bits 5-7: 110
      bits2 `shouldBe` 2 -- 10 binary = 2
      bits3 `shouldBe` 2 -- 010 binary = 2
      bits3' `shouldBe` 6 -- 110 binary = 6
    it "reads single bits correctly" $ do
      let bs = B.pack [0xFF]
          reader = initBitReader bs
          (bit1, reader') = readBit reader
          (bit2, reader'') = readBit reader'
          (bit3, _) = readBit reader''
      bit1 `shouldBe` True
      bit2 `shouldBe` True
      bit3 `shouldBe` True

    it "reads zero bits correctly" $ do
      let bs = B.pack [0x00]
          reader = initBitReader bs
          (bit1, reader') = readBit reader
          (bit2, _) = readBit reader'
      bit1 `shouldBe` False
      bit2 `shouldBe` False

    it "handles reading 0 bits" $ do
      let bs = B.pack [0xFF]
          reader = initBitReader bs
          (bits, _) = readBits 0 reader
      bits `shouldBe` 0

  describe "Buffer Refilling" $ do
    it "refills buffer automatically across bytes" $ do
      let bs = B.pack [0xFF, 0xAA, 0x55]
          reader = initBitReader bs
          (bits1, reader') = readBits 8 reader
          (bits2, reader'') = readBits 8 reader'
          (bits3, _) = readBits 8 reader''
      bits1 `shouldBe` 0xFF
      bits2 `shouldBe` 0xAA
      bits3 `shouldBe` 0x55

    it "handles reads spanning multiple bytes" $ do
      let bs = B.pack [0xFF, 0x00]
          reader = initBitReader bs
          (bits, _) = readBits 16 reader
      bits `shouldBe` 0x00FF -- LSB first
    it "reads sequential bytes correctly" $ do
      let bs = B.pack [0x12, 0x34]
          reader = initBitReader bs
          (byte1, reader') = readBits 8 reader
          (byte2, _) = readBits 8 reader'
      byte1 `shouldBe` 0x12
      byte2 `shouldBe` 0x34

  describe "Edge Cases" $ do
    it "handles empty input gracefully" $ do
      let bs = B.empty
          reader = initBitReader bs
          (bits, _) = readBits 8 reader
      bits `shouldBe` 0

    it "handles reading past end of data" $ do
      let bs = B.pack [0xFF]
          reader = initBitReader bs
          (_, reader') = readBits 8 reader
          (bits, _) = readBits 8 reader'
      bits `shouldBe` 0

    it "handles maximum bit reads (32 bits)" $ do
      let bs = B.pack [0xFF, 0xFF, 0xFF, 0xFF]
          reader = initBitReader bs
          (bits, _) = readBits 32 reader
      bits `shouldBe` 0xFFFFFFFF

    it "handles single byte input" $ do
      let bs = B.pack [0x55] -- 01010101
          reader = initBitReader bs
          (bit1, reader') = readBit reader
          (bit2, reader'') = readBit reader'
          (bit3, reader''') = readBit reader''
          (bit4, _) = readBit reader'''
      [bit1, bit2, bit3, bit4] `shouldBe` [True, False, True, False]

  describe "Byte Sequences" $ do
    it "correctly reads ascending byte sequence" $ do
      let bs = B.pack [0x01, 0x02, 0x03]
          reader = initBitReader bs
          (b1, r1) = readBits 8 reader
          (b2, r2) = readBits 8 r1
          (b3, _) = readBits 8 r2
      [b1, b2, b3] `shouldBe` [0x01, 0x02, 0x03]

    it "handles alternating bit patterns" $ do
      let bs = B.pack [0xAA, 0x55] -- 10101010 01010101
          reader = initBitReader bs
          (bits, _) = readBits 16 reader
      bits `shouldBe` 0x55AA -- LSB first
  describe "Bit Reconstruction" $ do
    it "reading 8 bits gives original byte" $ do
      let bs = B.pack [0xAB]
          reader = initBitReader bs
          (bits, _) = readBits 8 reader
      bits `shouldBe` 0xAB

    it "reading multiple 8-bit chunks gives original bytes" $ do
      let bs = B.pack [0x12, 0x34, 0x56]
          reader = initBitReader bs
          (b1, r1) = readBits 8 reader
          (b2, r2) = readBits 8 r1
          (b3, _) = readBits 8 r2
      [b1, b2, b3] `shouldBe` [0x12, 0x34, 0x56]

    it "splitting then combining bits preserves value" $ do
      let bs = B.pack [0xFF, 0xFF]
          reader = initBitReader bs
          (bits4, reader') = readBits 4 reader
          (bits12, _) = readBits 12 reader'
          combined = bits4 .|. (bits12 `shiftL` 4)
      combined `shouldBe` 0xFFFF

  describe "getBytesRemaining" $ do
    it "reports bytes not yet buffered initially" $ do
      let bs = B.pack [1, 2, 3, 4]
          reader = initBitReader bs
      -- After init, all 4 bytes are buffered, so 0 remain
      getBytesRemaining reader `shouldBe` 0

    it "reports bytes not yet buffered for large input" $ do
      let bs = B.pack [1 .. 20] -- 20 bytes
          reader = initBitReader bs
      -- After init, first 8 bytes are buffered, so 12 remain
      getBytesRemaining reader `shouldBe` 12

    it "handles empty input" $ do
      let bs = B.empty
          reader = initBitReader bs
      getBytesRemaining reader `shouldBe` 0
