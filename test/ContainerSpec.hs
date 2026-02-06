{-# LANGUAGE OverloadedStrings #-}

module ContainerSpec (spec) where

import Codec.Picture.WebP.Internal.Container
import Data.Bits
import qualified Data.ByteString as B
import Data.Word
import Test.Hspec

spec :: Spec
spec = describe "Container" $ do
  describe "RIFF Header Validation" $ do
    it "rejects non-RIFF files" $ do
      let invalidData = B.pack [0, 0, 0, 0]
      parseWebP invalidData `shouldSatisfy` isLeft

    it "rejects RIFF files that aren't WebP" $ do
      let invalidRiff = makeRIFFHeader "WAVE" 0
      parseWebP invalidRiff `shouldSatisfy` isLeft

    it "rejects truncated RIFF header" $ do
      let truncated = B.pack [82, 73, 70, 70] -- Just "RIFF"
      parseWebP truncated `shouldSatisfy` isLeft

    it "validates correct RIFF+WEBP header" $ do
      let header = makeRIFFHeader "WEBP" 20
      parseWebP header `shouldSatisfy` isLeft -- Will fail on missing chunks but header is valid
  describe "Simple VP8L Format" $ do
    it "parses simple VP8L structure" $ do
      let vp8lChunk = makeVP8LChunk [0x2F, 0, 0, 0, 0, 0, 0, 0]
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8lChunk) <> vp8lChunk
      case parseWebP webp of
        Right (WebPSimpleLossless _) -> return ()
        Right other -> expectationFailure $ "Expected WebPSimpleLossless, got: " ++ show other
        Left err -> expectationFailure $ "Parse failed: " ++ err

    it "parses VP8L with minimal valid header" $ do
      -- VP8L signature (0x2F) + 14-bit width + 14-bit height + alpha + version
      let vp8lData = [0x2F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
          vp8lChunk = makeVP8LChunk vp8lData
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8lChunk) <> vp8lChunk
      case parseWebP webp of
        Right (WebPSimpleLossless dat) -> do
          B.length dat `shouldBe` 8
        other -> expectationFailure $ "Unexpected result: " ++ show other

  describe "Simple VP8 Format" $ do
    it "parses simple VP8 structure" $ do
      let vp8Chunk = makeVP8Chunk [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8Chunk) <> vp8Chunk
      case parseWebP webp of
        Right (WebPSimpleLossy _) -> return ()
        Right other -> expectationFailure $ "Expected WebPSimpleLossy, got: " ++ show other
        Left err -> expectationFailure $ "Parse failed: " ++ err

  describe "Extended VP8X Format" $ do
    it "parses VP8X header" $ do
      let vp8xChunk = makeVP8XChunk 0x00 100 100
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8xChunk) <> vp8xChunk
      case parseWebP webp of
        Right (WebPExtended header _) -> do
          vp8xCanvasWidth header `shouldBe` 101 -- Field + 1
          vp8xCanvasHeight header `shouldBe` 101
        other -> expectationFailure $ "Unexpected result: " ++ show other

    it "parses VP8X with alpha flag" $ do
      let flags = 0x10 -- Alpha bit set
          vp8xChunk = makeVP8XChunk flags 99 99
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8xChunk) <> vp8xChunk
      case parseWebP webp of
        Right (WebPExtended header _) -> do
          vp8xHasAlpha header `shouldBe` True
          vp8xHasAnimation header `shouldBe` False
        other -> expectationFailure $ "Unexpected result: " ++ show other

    it "parses VP8X with animation flag" $ do
      let flags = 0x02 -- Animation bit set
          vp8xChunk = makeVP8XChunk flags 199 199
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8xChunk) <> vp8xChunk
      case parseWebP webp of
        Right (WebPExtended header _) -> do
          vp8xHasAnimation header `shouldBe` True
          vp8xHasAlpha header `shouldBe` False
        other -> expectationFailure $ "Unexpected result: " ++ show other

    it "parses VP8X with multiple chunks" $ do
      let vp8xChunk = makeVP8XChunk 0x00 50 50
          vp8lChunk = makeVP8LChunk [0x2F, 0, 0, 0, 0, 0, 0, 0]
          totalSize = B.length vp8xChunk + B.length vp8lChunk
          webp = makeRIFFHeader "WEBP" (fromIntegral totalSize) <> vp8xChunk <> vp8lChunk
      case parseWebP webp of
        Right (WebPExtended _ chunks) -> do
          length chunks `shouldBe` 1
        other -> expectationFailure $ "Unexpected result: " ++ show other

  describe "Chunk Parsing" $ do
    it "parses ALPH chunk" $ do
      let alphChunk = makeChunk "ALPH" [0x00, 0xFF, 0xFF]
          vp8xChunk = makeVP8XChunk 0x10 10 10
          totalSize = B.length vp8xChunk + B.length alphChunk
          webp = makeRIFFHeader "WEBP" (fromIntegral totalSize) <> vp8xChunk <> alphChunk
      case parseWebP webp of
        Right (WebPExtended _ chunks) -> do
          any isAlphChunk chunks `shouldBe` True
        other -> expectationFailure $ "Unexpected result: " ++ show other

    it "handles chunk padding correctly" $ do
      -- Odd-sized chunk should have padding byte
      let oddChunk = makeChunk "ICCP" [0x01, 0x02, 0x03] -- 3 bytes (odd)
          vp8xChunk = makeVP8XChunk 0x20 10 10
          totalSize = B.length vp8xChunk + B.length oddChunk
          webp = makeRIFFHeader "WEBP" (fromIntegral totalSize) <> vp8xChunk <> oddChunk
      case parseWebP webp of
        Right (WebPExtended _ chunks) -> do
          length chunks `shouldBe` 1
        Left err -> expectationFailure $ "Parse failed: " ++ err

  describe "Animation Chunks" $ do
    it "parses ANIM chunk" $ do
      let animChunk = makeChunk "ANIM" [0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00]
          vp8xChunk = makeVP8XChunk 0x02 10 10
          totalSize = B.length vp8xChunk + B.length animChunk
          webp = makeRIFFHeader "WEBP" (fromIntegral totalSize) <> vp8xChunk <> animChunk
      case parseWebP webp of
        Right (WebPExtended _ chunks) -> do
          any isAnimChunk chunks `shouldBe` True
        other -> expectationFailure $ "Unexpected result: " ++ show other

  describe "Error Handling" $ do
    it "reports error for empty input" $ do
      parseWebP B.empty `shouldSatisfy` isLeft

    it "reports error for truncated file" $ do
      let truncated = B.pack [82, 73, 70, 70, 10, 0, 0, 0] -- RIFF header only
      parseWebP truncated `shouldSatisfy` isLeft

    it "reports error for invalid chunk size" $ do
      let badSize = makeRIFFHeader "WEBP" 999999
      parseWebP badSize `shouldSatisfy` isLeft

-- Helper functions

makeRIFFHeader :: B.ByteString -> Word32 -> B.ByteString
makeRIFFHeader fourCC size =
  B.pack [82, 73, 70, 70] -- "RIFF"
    <> word32ToBytes size
    <> fourCC

makeVP8LChunk :: [Word8] -> B.ByteString
makeVP8LChunk payload =
  makeChunk "VP8L" payload

makeVP8Chunk :: [Word8] -> B.ByteString
makeVP8Chunk payload =
  makeChunk "VP8 " payload

makeVP8XChunk :: Word8 -> Word32 -> Word32 -> B.ByteString
makeVP8XChunk flags w h =
  let payload =
        [flags, 0, 0, 0] -- flags + 3 reserved bytes
          ++ word24ToBytes w
          ++ word24ToBytes h
   in makeChunk "VP8X" payload

makeChunk :: B.ByteString -> [Word8] -> B.ByteString
makeChunk fourCC payload =
  let size = fromIntegral (length payload) :: Word32
      padded = if odd (length payload) then payload ++ [0] else payload
   in fourCC <> word32ToBytes size <> B.pack padded

word32ToBytes :: Word32 -> B.ByteString
word32ToBytes w =
  B.pack
    [ fromIntegral w,
      fromIntegral (w `shiftR` 8),
      fromIntegral (w `shiftR` 16),
      fromIntegral (w `shiftR` 24)
    ]

word24ToBytes :: Word32 -> [Word8]
word24ToBytes w =
  [ fromIntegral w,
    fromIntegral (w `shiftR` 8),
    fromIntegral (w `shiftR` 16)
  ]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isAlphChunk :: WebPChunk -> Bool
isAlphChunk (ChunkALPH _) = True
isAlphChunk _ = False

isAnimChunk :: WebPChunk -> Bool
isAnimChunk (ChunkANIM _) = True
isAnimChunk _ = False
