{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PropertyExtendedSpec (spec) where

import Codec.Picture
import Codec.Picture.WebP
import qualified Data.ByteString as B
import Data.Word
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Extended Property-Based Tests" $ do
  describe "Dimension Invariants" $ do
    it "encoded width matches decoded width" $
      property $
        \(Positive w) ->
          let w' = (w `mod` 256) + 1
              img = generateImage (\_ _ -> PixelRGB8 128 128 128) w' 32
              encoded = encodeWebPLossy img 80
           in case decodeWebP encoded of
                Right (ImageRGB8 dec) -> imageWidth dec == w'
                _ -> False

    it "encoded height matches decoded height" $
      property $
        \(Positive h) ->
          let h' = (h `mod` 256) + 1
              img = generateImage (\_ _ -> PixelRGB8 128 128 128) 32 h'
              encoded = encodeWebPLossy img 80
           in case decodeWebP encoded of
                Right (ImageRGB8 dec) -> imageHeight dec == h'
                _ -> False

    it "arbitrary dimensions roundtrip correctly" $
      property $
        \(Positive w, Positive h) ->
          let w' = (w `mod` 128) + 1
              h' = (h `mod` 128) + 1
              img = generateImage (\_ _ -> PixelRGB8 100 100 100) w' h'
              encoded = encodeWebPLossy img 75
           in case decodeWebP encoded of
                Right (ImageRGB8 dec) -> imageWidth dec == w' && imageHeight dec == h'
                _ -> False

  describe "Color Preservation" $ do
    it "lossless preserves all pixel values" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8, a :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGBA8 r g b a) 16 16
              encoded = encodeWebPLossless img
           in case decodeWebP encoded of
                Right (ImageRGBA8 dec) ->
                  let PixelRGBA8 r' g' b' a' = pixelAt dec 8 8
                   in r' == r && g' == g && b' == b && a' == a
                _ -> False

    it "lossy approximates colors within tolerance" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGB8 r g b) 32 32
              encoded = encodeWebPLossy img 85
           in case decodeWebP encoded of
                Right (ImageRGB8 dec) ->
                  let PixelRGB8 r' g' b' = pixelAt dec 16 16
                      dr = abs (fromIntegral r' - fromIntegral r :: Int)
                      dg = abs (fromIntegral g' - fromIntegral g :: Int)
                      db = abs (fromIntegral b' - fromIntegral b :: Int)
                   in dr < 50 && dg < 50 && db < 50
                _ -> False

    it "alpha is preserved exactly in lossy+alpha mode" $
      property $
        \(a :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGBA8 128 128 128 a) 16 16
              encoded = encodeWebPLossyWithAlpha img 80
           in case decodeWebP encoded of
                Right (ImageRGBA8 dec) ->
                  let PixelRGBA8 _ _ _ a' = pixelAt dec 8 8
                   in a' == a
                _ -> False

  describe "File Format Invariants" $ do
    it "all files start with RIFF" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGB8 r g b) 16 16
              encoded = encodeWebPLossy img 80
           in B.length encoded >= 4 && B.take 4 encoded == B.pack [82, 73, 70, 70]

    it "all files contain WEBP" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGB8 r g b) 16 16
              encoded = encodeWebPLossy img 80
           in B.length encoded >= 12 && B.take 4 (B.drop 8 encoded) == B.pack [87, 69, 66, 80]

    it "lossless files contain VP8L marker" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8, a :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGBA8 r g b a) 16 16
              encoded = encodeWebPLossless img
           in B.length encoded >= 16 && B.take 4 (B.drop 12 encoded) == B.pack [86, 80, 56, 76]

    it "file size is always positive" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGB8 r g b) 16 16
              encoded = encodeWebPLossy img 80
           in B.length encoded > 0

  describe "Encoding Determinism" $ do
    it "same image produces identical bytes" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGB8 r g b) 24 24
              enc1 = encodeWebPLossy img 80
              enc2 = encodeWebPLossy img 80
           in enc1 == enc2

    it "lossless is deterministic" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8, a :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGBA8 r g b a) 24 24
              enc1 = encodeWebPLossless img
              enc2 = encodeWebPLossless img
           in enc1 == enc2

  describe "Quality Effects" $ do
    it "higher quality often produces larger files" $
      property $
        \(seed :: Int) ->
          let img =
                generateImage
                  ( \x y ->
                      PixelRGB8
                        (fromIntegral $ (x + seed) `mod` 256)
                        (fromIntegral $ (y + seed) `mod` 256)
                        128
                  )
                  64
                  64
              low = encodeWebPLossy img 10
              high = encodeWebPLossy img 90
           in B.length high >= B.length low - 100 -- Allow some tolerance
    it "quality 0-100 all produce valid output" $
      property $
        \(Positive q) ->
          let q' = q `mod` 101
              img = generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16
              encoded = encodeWebPLossy img q'
           in case decodeWebP encoded of
                Right (ImageRGB8 _) -> True
                _ -> False

  describe "Idempotence Properties" $ do
    it "lossless encode-decode is approximately idempotent" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8, a :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGBA8 r g b a) 16 16
              enc1 = encodeWebPLossless img
           in case decodeWebP enc1 of
                Right (ImageRGBA8 dec1) ->
                  let enc2 = encodeWebPLossless dec1
                   in case decodeWebP enc2 of
                        Right (ImageRGBA8 dec2) ->
                          let PixelRGBA8 r1 g1 b1 a1 = pixelAt dec1 8 8
                              PixelRGBA8 r2 g2 b2 a2 = pixelAt dec2 8 8
                           in r1 == r2 && g1 == g2 && b1 == b2 && a1 == a2
                        _ -> False
                _ -> False

  describe "Pixel Value Bounds" $ do
    it "decoded pixel values are in valid range" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGB8 r g b) 32 32
              encoded = encodeWebPLossy img 80
           in case decodeWebP encoded of
                Right (ImageRGB8 dec) ->
                  let PixelRGB8 r' g' b' = pixelAt dec 16 16
                   in r' <= 255 && g' <= 255 && b' <= 255
                _ -> False

    it "lossless decoded values are exact" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8, a :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGBA8 r g b a) 16 16
              encoded = encodeWebPLossless img
           in case decodeWebP encoded of
                Right (ImageRGBA8 dec) ->
                  pixelAt dec 8 8 == PixelRGBA8 r g b a
                _ -> False

  describe "Grayscale Handling" $ do
    it "grayscale images encode correctly" $
      property $
        \(v :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGB8 v v v) 32 32
              encoded = encodeWebPLossy img 80
           in case decodeWebP encoded of
                Right (ImageRGB8 dec) ->
                  let PixelRGB8 r g b = pixelAt dec 16 16
                      maxDiff = maximum [abs (fromIntegral r - fromIntegral v), abs (fromIntegral g - fromIntegral v), abs (fromIntegral b - fromIntegral v)] :: Int
                   in maxDiff < 20
                _ -> False

    it "black and white survive lossy compression" $
      property $
        \(isWhite :: Bool) ->
          let v = if isWhite then 255 else 0
              img = generateImage (\_ _ -> PixelRGB8 v v v) 32 32
              encoded = encodeWebPLossy img 90
           in case decodeWebP encoded of
                Right (ImageRGB8 dec) ->
                  let PixelRGB8 r g b = pixelAt dec 16 16
                      expected = if isWhite then 255 else 0 :: Int
                      avgVal = (fromIntegral r + fromIntegral g + fromIntegral b) `div` 3
                   in abs (avgVal - expected) < 30
                _ -> False

  describe "Animation Properties" $ do
    it "frame count is preserved" $
      property $
        \(Positive n) ->
          let n' = (n `mod` 10) + 1
              frames =
                [ WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 (fromIntegral i) 0 0) 8 8) 50 0 0
                | i <- [0 .. n' - 1]
                ]
              encoded = encodeWebPAnimation frames 8 8 70
           in case decodeWebPAnimation encoded of
                Right decodedFrames -> length decodedFrames == n'
                Left _ -> False

    it "frame durations are preserved" $
      property $
        \(Positive d) ->
          let d' = (d `mod` 1000) + 1
              frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 128 128 128) 8 8) d' 0 0
              encoded = encodeWebPAnimation [frame] 8 8 70
           in case decodeWebPAnimation encoded of
                Right [f] -> webpFrameDuration f == d'
                _ -> False

  describe "Compression Ratio" $ do
    it "solid colors compress well" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8) ->
          let img = generateImage (\_ _ -> PixelRGB8 r g b) 64 64
              encoded = encodeWebPLossy img 80
              rawSize = 64 * 64 * 3
           in B.length encoded < rawSize

    it "file size is bounded" $
      property $
        \(r :: Word8, g :: Word8, b :: Word8, Positive w, Positive h) ->
          let w' = (w `mod` 64) + 8
              h' = (h `mod` 64) + 8
              img = generateImage (\_ _ -> PixelRGB8 r g b) w' h'
              encoded = encodeWebPLossy img 80
              -- File should be smaller than raw + overhead
              maxSize = w' * h' * 3 + 1000
           in B.length encoded < maxSize
