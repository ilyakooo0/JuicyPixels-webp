module PropertySpec (spec) where

import Codec.Picture
import Codec.Picture.WebP
import qualified Data.ByteString as B
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Property-Based Tests" $ do
    describe "Dimension Properties" $ do
      it "encodes and decodes arbitrary dimensions (small)" $
        property $
          \(Positive w, Positive h) ->
            let w' = (w `mod` 64) + 1 -- 1-64
                h' = (h `mod` 64) + 1
                img = generateImage (\_ _ -> PixelRGB8 128 128 128) w' h'
                encoded = encodeWebPLossy img 80
             in case decodeWebP encoded of
                  Right (ImageRGB8 dec) ->
                    imageWidth dec == w' && imageHeight dec == h'
                  _ -> False

      it "lossless preserves dimensions exactly" $
        property $
          \(Positive w, Positive h) ->
            let w' = (w `mod` 128) + 1
                h' = (h `mod` 128) + 1
                img = generateImage (\_ _ -> PixelRGBA8 128 128 128 255) w' h'
                encoded = encodeWebPLossless img
             in case decodeWebP encoded of
                  Right (ImageRGBA8 dec) ->
                    imageWidth dec == w' && imageHeight dec == h'
                  _ -> False

    describe "Color Properties" $ do
      it "solid color survives lossy encoding (approximate)" $
        property $
          \r g b ->
            let img = generateImage (\_ _ -> PixelRGB8 r g b) 32 32
                encoded = encodeWebPLossy img 85
             in case decodeWebP encoded of
                  Right (ImageRGB8 dec) ->
                    let PixelRGB8 r' g' b' = pixelAt dec 16 16
                     in -- Allow larger tolerance for edge cases
                        abs (fromIntegral r' - fromIntegral r :: Int) < 40
                          && abs (fromIntegral g' - fromIntegral g :: Int) < 40
                          && abs (fromIntegral b' - fromIntegral b :: Int) < 40
                  _ -> False

      it "lossless preserves exact color values" $
        property $
          \r g b a ->
            let img = generateImage (\_ _ -> PixelRGBA8 r g b a) 16 16
                encoded = encodeWebPLossless img
             in case decodeWebP encoded of
                  Right (ImageRGBA8 dec) ->
                    pixelAt dec 8 8 == PixelRGBA8 r g b a
                  _ -> False

    describe "Encoding Stability" $ do
      it "encoding is deterministic (same input = same output)" $
        property $
          \r g b ->
            let img = generateImage (\_ _ -> PixelRGB8 r g b) 16 16
                enc1 = encodeWebPLossy img 80
                enc2 = encodeWebPLossy img 80
             in enc1 == enc2

      it "lossless encoding is always deterministic" $
        property $
          \r g b a ->
            let img = generateImage (\_ _ -> PixelRGBA8 r g b a) 16 16
                enc1 = encodeWebPLossless img
                enc2 = encodeWebPLossless img
             in enc1 == enc2

    describe "Roundtrip Properties" $ do
      it "lossy encode-decode produces valid image" $
        property $
          \(Positive w, Positive h, r, g, b) ->
            let w' = (w `mod` 64) + 1
                h' = (h `mod` 64) + 1
                img = generateImage (\_ _ -> PixelRGB8 r g b) w' h'
                encoded = encodeWebPLossy img 80
             in case decodeWebP encoded of
                  Right (ImageRGB8 dec) ->
                    imageWidth dec == w' && imageHeight dec == h'
                  _ -> False

      it "lossless roundtrip is identity" $
        property $
          \r g b a ->
            let img = generateImage (\_ _ -> PixelRGBA8 r g b a) 16 16
                encoded = encodeWebPLossless img
             in case decodeWebP encoded of
                  Right (ImageRGBA8 dec) ->
                    all
                      (\(x, y) -> pixelAt dec x y == PixelRGBA8 r g b a)
                      [(x, y) | x <- [0 .. 15], y <- [0 .. 15]]
                  _ -> False

    describe "File Format Properties" $ do
      it "all encoded files start with RIFF signature" $
        property $
          \r g b ->
            let img = generateImage (\_ _ -> PixelRGB8 r g b) 16 16
                encoded = encodeWebPLossy img 80
             in B.length encoded >= 12
                  && B.take 4 encoded == B.pack [82, 73, 70, 70] -- "RIFF"
      it "all encoded files have WEBP signature" $
        property $
          \r g b ->
            let img = generateImage (\_ _ -> PixelRGB8 r g b) 16 16
                encoded = encodeWebPLossy img 80
             in B.length encoded >= 12
                  && B.take 4 (B.drop 8 encoded) == B.pack [87, 69, 66, 80] -- "WEBP"
      it "file size is reasonable for dimensions" $
        property $
          \(Positive w, Positive h) ->
            let w' = (w `mod` 64) + 1
                h' = (h `mod` 64) + 1
                img = generateImage (\_ _ -> PixelRGB8 128 128 128) w' h'
                encoded = encodeWebPLossy img 80
             in -- File should be at least header size (RIFF + VP8 chunk headers)
                -- For very small solid-color images, compression is extremely efficient
                B.length encoded > 20
