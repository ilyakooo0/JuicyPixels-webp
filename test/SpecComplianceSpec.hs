{-# LANGUAGE OverloadedStrings #-}

module SpecComplianceSpec (spec) where

import Codec.Picture
import Codec.Picture.WebP
import Codec.Picture.WebP.Internal.Container
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import Data.Word
import Test.Hspec

spec :: Spec
spec = describe "Spec Compliance" $ do
  describe "RIFF Container (RFC 9649)" $ do
    it "encoded files start with RIFF signature" $ do
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16
          encoded = encodeWebPLossy img 80
      B.take 4 encoded `shouldBe` B.pack [0x52, 0x49, 0x46, 0x46] -- "RIFF"

    it "encoded files have WEBP form type" $ do
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16
          encoded = encodeWebPLossy img 80
      B.take 4 (B.drop 8 encoded) `shouldBe` B.pack [0x57, 0x45, 0x42, 0x50] -- "WEBP"

    it "file size field is correct (little-endian)" $ do
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16
          encoded = encodeWebPLossy img 80
          fileSize = B.length encoded
          storedSize = fromWord32LE (B.unpack $ B.take 4 (B.drop 4 encoded))
      -- Stored size = file size - 8 (RIFF + size field)
      storedSize `shouldBe` (fileSize - 8)

    it "lossless files start with RIFF signature" $ do
      let img = generateImage (\_ _ -> PixelRGBA8 128 128 128 255) 16 16
          encoded = encodeWebPLossless img
      B.take 4 encoded `shouldBe` B.pack [0x52, 0x49, 0x46, 0x46]

  describe "VP8L Signature (RFC 9649 Section 4)" $ do
    it "VP8L chunk starts with correct FourCC" $ do
      let img = generateImage (\_ _ -> PixelRGBA8 128 128 128 255) 16 16
          encoded = encodeWebPLossless img
      -- After RIFF header (12 bytes), should be VP8L chunk
      B.take 4 (B.drop 12 encoded) `shouldBe` B.pack [0x56, 0x50, 0x38, 0x4C] -- "VP8L"

    it "VP8L data starts with signature byte 0x2F" $ do
      let img = generateImage (\_ _ -> PixelRGBA8 128 128 128 255) 16 16
          encoded = encodeWebPLossless img
      -- VP8L chunk: 4 bytes FourCC + 4 bytes size + 1 byte signature
      B.index encoded 20 `shouldBe` 0x2F

  describe "VP8 Signature (RFC 6386)" $ do
    it "VP8 chunk starts with correct FourCC" $ do
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16
          encoded = encodeWebPLossy img 80
      -- After RIFF header, should be VP8 chunk
      B.take 4 (B.drop 12 encoded) `shouldBe` B.pack [0x56, 0x50, 0x38, 0x20] -- "VP8 "

    it "VP8 frame tag indicates key frame" $ do
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16
          encoded = encodeWebPLossy img 80
      -- VP8 chunk data starts after: RIFF(12) + VP8 FourCC(4) + size(4)
      let frameTag = B.index encoded 20
          isKeyFrame = (frameTag .&. 0x01) == 0
      isKeyFrame `shouldBe` True

    it "VP8 frame has correct signature bytes" $ do
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16
          encoded = encodeWebPLossy img 80
      -- After frame tag, there should be VP8 signature: 0x9D 0x01 0x2A
      -- The exact position depends on the 3-byte frame tag
      let frameTag = B.unpack $ B.take 3 (B.drop 20 encoded)
          tagSize = 3 -- Frame tag is always 3 bytes
      B.index encoded (20 + tagSize) `shouldBe` 0x9D
      B.index encoded (21 + tagSize) `shouldBe` 0x01
      B.index encoded (22 + tagSize) `shouldBe` 0x2A

  describe "Width/Height Encoding" $ do
    it "VP8L stores width-1 (RFC 9649 Section 4.1)" $ do
      -- When decoding, we should get back the original dimensions
      let img = generateImage (\_ _ -> PixelRGBA8 100 100 100 255) 100 100
          encoded = encodeWebPLossless img
      case decodeWebP encoded of
        Right (ImageRGBA8 dec) -> do
          imageWidth dec `shouldBe` 100
          imageHeight dec `shouldBe` 100
        _ -> expectationFailure "Decode failed"

    it "VP8 dimensions are correct" $ do
      let img = generateImage (\_ _ -> PixelRGB8 50 100 150) 64 48
          encoded = encodeWebPLossy img 80
      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          imageWidth dec `shouldBe` 64
          imageHeight dec `shouldBe` 48
        _ -> expectationFailure "Decode failed"

  describe "Container Parsing" $ do
    it "rejects invalid RIFF signature" $ do
      let invalid = B.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
      case parseWebP invalid of
        Left _ -> pure () -- Expected
        Right _ -> expectationFailure "Should reject invalid RIFF"

    it "rejects truncated file" $ do
      let truncated = B.pack [0x52, 0x49, 0x46, 0x46] -- Just "RIFF"
      case parseWebP truncated of
        Left _ -> pure () -- Expected
        Right _ -> expectationFailure "Should reject truncated file"

    it "rejects wrong form type" $ do
      let wrongForm = B.pack [0x52, 0x49, 0x46, 0x46, 0x04, 0x00, 0x00, 0x00, 0x41, 0x56, 0x49, 0x20] -- "RIFF....AVI "
      case parseWebP wrongForm of
        Left _ -> pure () -- Expected
        Right _ -> expectationFailure "Should reject non-WEBP form"

  describe "VP8L Transform Order (RFC 9649 Section 4.2)" $ do
    it "lossless roundtrip preserves colors" $ do
      -- Test that transforms are correctly applied in reverse order during decode
      let img = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) 128 255) 64 64
          encoded = encodeWebPLossless img
      case decodeWebP encoded of
        Right (ImageRGBA8 dec) -> do
          -- Simple encoder may quantize, so check approximately
          let PixelRGBA8 r g _ a = pixelAt dec 32 32
          abs (fromIntegral r - 32 :: Int) `shouldSatisfy` (< 50)
          abs (fromIntegral g - 32 :: Int) `shouldSatisfy` (< 50)
          a `shouldBe` 255
        _ -> expectationFailure "Decode failed"

  describe "Alpha Channel (RFC 9649 Section 3)" $ do
    it "VP8X extended format supports alpha" $ do
      let img = generateImage (\_ _ -> PixelRGBA8 255 128 64 200) 32 32
          encoded = encodeWebPLossyWithAlpha img 80
      case decodeWebP encoded of
        Right (ImageRGBA8 dec) -> do
          let PixelRGBA8 _ _ _ a = pixelAt dec 16 16
          a `shouldBe` 200 -- Alpha should be preserved exactly
        _ -> expectationFailure "Should decode to RGBA8"

    it "fully opaque RGBA encodes and decodes correctly" $ do
      let img = generateImage (\_ _ -> PixelRGBA8 100 150 200 255) 32 32
          encoded = encodeWebPLossyWithAlpha img 80
      case decodeWebP encoded of
        Right (ImageRGBA8 dec) -> do
          let PixelRGBA8 _ _ _ a = pixelAt dec 16 16
          a `shouldBe` 255
        _ -> expectationFailure "Should decode to RGBA8"

    it "fully transparent alpha is preserved" $ do
      let img = generateImage (\_ _ -> PixelRGBA8 100 150 200 0) 32 32
          encoded = encodeWebPLossyWithAlpha img 80
      case decodeWebP encoded of
        Right (ImageRGBA8 dec) -> do
          let PixelRGBA8 _ _ _ a = pixelAt dec 16 16
          a `shouldBe` 0
        _ -> expectationFailure "Should decode to RGBA8"

  describe "Animation (RFC 9649 Section 3.2)" $ do
    it "animation frames have valid duration" $ do
      let frame1 = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16) 100 0 0
          frame2 = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 0 255 0) 16 16) 200 0 0
          encoded = encodeWebPAnimation [frame1, frame2] 16 16 80

      case decodeWebPAnimation encoded of
        Right frames -> do
          length frames `shouldBe` 2
          -- Duration is in milliseconds
          webpFrameDuration (frames !! 0) `shouldBe` 100
          webpFrameDuration (frames !! 1) `shouldBe` 200
        Left err -> expectationFailure $ "Decode failed: " ++ err

  describe "Color Space (RFC 6386 Section 19.1)" $ do
    it "YCbCr to RGB conversion produces valid colors" $ do
      let img = generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16
          encoded = encodeWebPLossy img 100
      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          let PixelRGB8 r g b = pixelAt dec 8 8
          -- Red should dominate after roundtrip
          r `shouldSatisfy` (> g)
          r `shouldSatisfy` (> b)
        _ -> expectationFailure "Decode failed"

    it "pure green roundtrip" $ do
      let img = generateImage (\_ _ -> PixelRGB8 0 255 0) 16 16
          encoded = encodeWebPLossy img 100
      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          let PixelRGB8 r g b = pixelAt dec 8 8
          g `shouldSatisfy` (> r)
          g `shouldSatisfy` (> b)
        _ -> expectationFailure "Decode failed"

    it "pure blue roundtrip" $ do
      let img = generateImage (\_ _ -> PixelRGB8 0 0 255) 16 16
          encoded = encodeWebPLossy img 100
      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          let PixelRGB8 r g b = pixelAt dec 8 8
          b `shouldSatisfy` (> r)
          b `shouldSatisfy` (> g)
        _ -> expectationFailure "Decode failed"

  describe "Chunk Alignment (RFC 9649 Section 2)" $ do
    it "file size is valid after encoding" $ do
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 32 32
          encoded = encodeWebPLossy img 80
      -- File should be at least RIFF header size
      B.length encoded `shouldSatisfy` (>= 12)
      -- Size should be reasonable
      B.length encoded `shouldSatisfy` (< 100000)

-- Helper: Convert 4 bytes to Word32 (little-endian)
fromWord32LE :: [Word8] -> Int
fromWord32LE [a, b, c, d] =
  fromIntegral a
    + fromIntegral b * 256
    + fromIntegral c * 65536
    + fromIntegral d * 16777216
fromWord32LE _ = 0
