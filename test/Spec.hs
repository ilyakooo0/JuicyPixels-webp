{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Codec.Picture.WebP
import Codec.Picture.WebP.Internal.BitReader
import Codec.Picture.WebP.Internal.Container
import Codec.Picture.WebP.Internal.VP8L.PrefixCode
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = hspec $ do
  describe "BitReader" bitReaderTests
  describe "Container" containerTests
  describe "PrefixCode" prefixCodeTests
  describe "Public API" publicAPITests

bitReaderTests :: Spec
bitReaderTests = do
  it "reads bits LSB-first" $ do
    let bs = B.pack [0xCA]
        reader = initBitReader bs
        (bits2, reader') = readBits 2 reader
        (bits3, reader'') = readBits 3 reader'
    bits2 `shouldBe` 2
    bits3 `shouldBe` 2

  it "reads single bits" $ do
    let bs = B.pack [0xFF]
        reader = initBitReader bs
        (bit1, reader') = readBit reader
        (bit2, _) = readBit reader'
    bit1 `shouldBe` True
    bit2 `shouldBe` True

  it "refills buffer automatically" $ do
    let bs = B.pack [0xFF, 0xAA]
        reader = initBitReader bs
        (bits1, reader') = readBits 8 reader
        (bits2, _) = readBits 8 reader'
    bits1 `shouldBe` 0xFF
    bits2 `shouldBe` 0xAA

containerTests :: Spec
containerTests = do
  it "rejects non-RIFF files" $ do
    let invalidData = B.pack [0, 0, 0, 0]
    parseWebP invalidData `shouldSatisfy` isLeft

  it "rejects non-WebP RIFF files" $ do
    let invalidRiff = B.pack $ [82, 73, 70, 70]
                            ++ [0, 0, 0, 0]
                            ++ [78, 79, 84, 87]
    parseWebP invalidRiff `shouldSatisfy` isLeft

  it "parses simple VP8L structure" $ do
    let header = B.pack $ [82, 73, 70, 70]
                       ++ [20, 0, 0, 0]
                       ++ [87, 69, 66, 80]
                       ++ [86, 80, 56, 76]
                       ++ [8, 0, 0, 0]
                       ++ [0x2F, 0, 0, 0, 0, 0, 0, 0]
    case parseWebP header of
      Right (WebPSimpleLossless _) -> return ()
      _ -> expectationFailure "Expected WebPSimpleLossless"

prefixCodeTests :: Spec
prefixCodeTests = do
  it "builds single-symbol code" $ do
    let lengths = VU.fromList [0, 0, 0, 0]
    case buildPrefixCode lengths of
      Left _ -> return ()
      Right (PrefixCodeSingle sym) -> sym `shouldBe` 0
      Right _ -> expectationFailure "Expected single symbol code"

  it "builds simple two-symbol code" $ do
    let lengths = VU.fromList [1, 1]
    case buildPrefixCode lengths of
      Left _ -> expectationFailure "Should build code"
      Right (PrefixCodeTable _ _) -> return ()
      Right _ -> return ()

  it "rejects invalid code lengths" $ do
    let lengths = VU.fromList []
    buildPrefixCode lengths `shouldSatisfy` isLeft

publicAPITests :: Spec
publicAPITests = do
  it "handles empty input" $ do
    let emptyData = B.empty
    case decodeWebP emptyData of
      Left _ -> return ()
      Right _ -> expectationFailure "Should fail on empty input"

  it "reports clear error for invalid data" $ do
    let invalidData = B.pack [1, 2, 3, 4]
    case decodeWebP invalidData of
      Left err -> err `shouldContain` "RIFF"
      Right _ -> expectationFailure "Should fail on invalid data"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
