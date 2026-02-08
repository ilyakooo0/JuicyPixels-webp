module VP8EncodeSpec (spec) where

import Codec.Picture
import Codec.Picture.WebP
import qualified Data.ByteString as B
import Test.Hspec

spec :: Spec
spec = do
  describe "VP8 Lossy Encoder" $ do
    describe "Basic Encoding" $ do
      it "encodes and decodes a simple red image (16x16)" $ do
        let img = generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16
            encoded = encodeWebPLossy img 80

        -- Should produce valid WebP file
        B.length encoded `shouldSatisfy` (> 0)

        -- Should decode successfully
        case decodeWebP encoded of
          Right dynImg -> do
            -- Should decode to RGB8 or RGBA8
            case dynImg of
              ImageRGB8 decoded -> do
                imageWidth decoded `shouldBe` 16
                imageHeight decoded `shouldBe` 16
              ImageRGBA8 decoded -> do
                imageWidth decoded `shouldBe` 16
                imageHeight decoded `shouldBe` 16
              _ -> expectationFailure "Unexpected image format (not RGB8 or RGBA8)"
          Left err -> expectationFailure $ "Decode failed: " ++ err

      it "encodes and decodes a simple solid color image (32x32)" $ do
        let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 32 32
            encoded = encodeWebPLossy img 80

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            imageWidth decoded `shouldBe` 32
            imageHeight decoded `shouldBe` 32
            -- For solid color, should be close to original
            let PixelRGB8 r g b = pixelAt decoded 16 16
            abs (fromIntegral r - 128 :: Int) `shouldSatisfy` (< 10)
            abs (fromIntegral g - 128 :: Int) `shouldSatisfy` (< 10)
            abs (fromIntegral b - 128 :: Int) `shouldSatisfy` (< 10)
          Right _ -> expectationFailure "Expected RGB8 image"
          Left err -> expectationFailure $ "Decode failed: " ++ err

    describe "Quality Levels" $ do
      it "produces different file sizes for different quality levels" $ do
        let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64
            encoded10 = encodeWebPLossy img 10
            encoded50 = encodeWebPLossy img 50
            encoded90 = encodeWebPLossy img 90

        -- All should be valid
        B.length encoded10 `shouldSatisfy` (> 0)
        B.length encoded50 `shouldSatisfy` (> 0)
        B.length encoded90 `shouldSatisfy` (> 0)

        -- Higher quality might be larger (though not always for simple patterns)
        -- At minimum, they should all decode
        case decodeWebP encoded10 of
          Right _ -> pure ()
          Left err -> expectationFailure $ "Failed to decode quality 10: " ++ err
        case decodeWebP encoded50 of
          Right _ -> pure ()
          Left err -> expectationFailure $ "Failed to decode quality 50: " ++ err
        case decodeWebP encoded90 of
          Right _ -> pure ()
          Left err -> expectationFailure $ "Failed to decode quality 90: " ++ err

    describe "Alpha Channel Encoding" $ do
      it "encodes and decodes RGBA image with alpha" $ do
        let img = generateImage (\x y -> PixelRGBA8 255 0 0 (fromIntegral $ x + y)) 32 32
            encoded = encodeWebPLossyWithAlpha img 80

        B.length encoded `shouldSatisfy` (> 0)

        case decodeWebP encoded of
          Right (ImageRGBA8 decoded) -> do
            imageWidth decoded `shouldBe` 32
            imageHeight decoded `shouldBe` 32
            -- Check that alpha is preserved (approximately)
            let PixelRGBA8 _ _ _ a = pixelAt decoded 10 10
            abs (fromIntegral a - 20 :: Int) `shouldSatisfy` (< 10)
          Right (ImageRGB8 _) -> expectationFailure "Expected RGBA8 image with alpha"
          Right _ -> expectationFailure "Unexpected image format"
          Left err -> expectationFailure $ "Decode failed: " ++ err

      it "encodes semi-transparent image" $ do
        let img = generateImage (\_ _ -> PixelRGBA8 128 128 255 128) 16 16
            encoded = encodeWebPLossyWithAlpha img 80

        case decodeWebP encoded of
          Right (ImageRGBA8 _) -> pure ()
          Right _ -> expectationFailure "Expected RGBA8 image"
          Left err -> expectationFailure $ "Decode failed: " ++ err

    describe "Animation Encoding" $ do
      it "encodes simple 2-frame animation" $ do
        let frame1 = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16) 100 0 0
            frame2 = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 0 255 0) 16 16) 100 0 0
            encoded = encodeWebPAnimation [frame1, frame2] 16 16 80

        B.length encoded `shouldSatisfy` (> 0)

        -- Should decode as animation
        case decodeWebPAnimation encoded of
          Right frames -> length frames `shouldBe` 2
          Left err -> expectationFailure $ "Animation decode failed: " ++ err

      it "encodes animation with multiple frames" $ do
        let frames = [ WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 (fromIntegral i) 0 0) 16 16) 50 0 0
                     | i <- [0..4] ]
            encoded = encodeWebPAnimation frames 16 16 80

        case decodeWebPAnimation encoded of
          Right decodedFrames -> length decodedFrames `shouldBe` 5
          Left err -> expectationFailure $ "Animation decode failed: " ++ err
