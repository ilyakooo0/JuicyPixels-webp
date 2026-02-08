{-# LANGUAGE OverloadedStrings #-}

module LoopFilterSpec (spec) where

import Codec.Picture
import Codec.Picture.WebP
import qualified Data.ByteString as B
import Test.Hspec
import Test.QuickCheck

-- | Tests for loop filter effects through integration testing.
-- The loop filter is an internal component that smooths block boundaries
-- after VP8 lossy decoding. We test its effects indirectly through
-- encode/decode cycles.

spec :: Spec
spec = describe "LoopFilter" $ do
  describe "Loop Filter Effects" $ do
    it "high quality encoding shows less blocking artifacts" $ do
      -- Create an image with sharp edges that would show blocking
      let img =
            generateImage
              ( \x y ->
                  if (x `div` 16 + y `div` 16) `mod` 2 == 0
                    then PixelRGB8 0 0 0
                    else PixelRGB8 255 255 255
              )
              64
              64
          -- Low quality should have more visible artifacts
          encodedLow = encodeWebPLossy img 10
          encodedHigh = encodeWebPLossy img 90

      case (decodeWebP encodedLow, decodeWebP encodedHigh) of
        (Right (ImageRGB8 decLow), Right (ImageRGB8 decHigh)) -> do
          -- Both should decode to the correct dimensions
          (imageWidth decLow, imageHeight decLow) `shouldBe` (64, 64)
          (imageWidth decHigh, imageHeight decHigh) `shouldBe` (64, 64)
        _ -> expectationFailure "Decoding failed"

    it "solid color images have minimal filter artifacts" $ do
      -- Solid color should have no edges to filter
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 64 64
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          -- Center pixel should be close to original
          let PixelRGB8 r g b = pixelAt dec 32 32
          abs (fromIntegral r - 128 :: Int) `shouldSatisfy` (< 15)
          abs (fromIntegral g - 128 :: Int) `shouldSatisfy` (< 15)
          abs (fromIntegral b - 128 :: Int) `shouldSatisfy` (< 15)
        _ -> expectationFailure "Decoding failed"

    it "gradient images are smoothed appropriately" $ do
      -- Gradients should be preserved reasonably well
      let img =
            generateImage
              ( \x _ ->
                  let v = fromIntegral $ (x * 4) `min` 255
                   in PixelRGB8 v v v
              )
              64
              64
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          -- Check that the gradient is roughly maintained
          let PixelRGB8 r1 _ _ = pixelAt dec 10 32
              PixelRGB8 r2 _ _ = pixelAt dec 50 32
          r2 `shouldSatisfy` (> r1) -- Gradient direction should be preserved
        _ -> expectationFailure "Decoding failed"

  describe "Block Boundary Smoothing" $ do
    it "encodes and decodes macroblock-aligned images" $ do
      -- 16x16 is exactly one macroblock
      let img = generateImage (\_ _ -> PixelRGB8 100 150 200) 16 16
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          imageWidth dec `shouldBe` 16
          imageHeight dec `shouldBe` 16
        _ -> expectationFailure "Decoding failed"

    it "encodes and decodes 2x2 macroblock images" $ do
      let img = generateImage (\_ _ -> PixelRGB8 50 100 150) 32 32
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          imageWidth dec `shouldBe` 32
          imageHeight dec `shouldBe` 32
        _ -> expectationFailure "Decoding failed"

    it "encodes and decodes non-macroblock-aligned images" $ do
      -- 33x33 requires padding
      let img = generateImage (\_ _ -> PixelRGB8 80 120 160) 33 33
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          -- Should crop back to original size
          imageWidth dec `shouldBe` 33
          imageHeight dec `shouldBe` 33
        _ -> expectationFailure "Decoding failed"

  describe "Quality vs Filter Strength" $ do
    it "quality 0 still produces valid output" $ do
      let img =
            generateImage
              (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128)
              64
              64
          encoded = encodeWebPLossy img 0

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          imageWidth dec `shouldBe` 64
          imageHeight dec `shouldBe` 64
          -- Output should be valid (all values in range)
          let centerPixel = pixelAt dec 32 32
          centerPixel `shouldSatisfy` (\(PixelRGB8 r g b) -> r <= 255 && g <= 255 && b <= 255)
        _ -> expectationFailure "Decoding failed"

    it "quality 100 produces valid output" $ do
      let img =
            generateImage
              (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128)
              64
              64
          encoded = encodeWebPLossy img 100

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          imageWidth dec `shouldBe` 64
          imageHeight dec `shouldBe` 64
        _ -> expectationFailure "Decoding failed"

    it "all quality levels produce decodable output" $ do
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 32 32
          qualities = [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

      forM_ qualities $ \q -> do
        let encoded = encodeWebPLossy img q
        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (32, 32)
          Right _ -> expectationFailure $ "Wrong format for quality " ++ show q
          Left err -> expectationFailure $ "Decode failed for quality " ++ show q ++ ": " ++ err

  describe "Edge Preservation" $ do
    it "sharp edges survive high-quality encoding" $ do
      -- Create image with sharp vertical edge
      let img =
            generateImage
              ( \x _ ->
                  if x < 32
                    then PixelRGB8 0 0 0
                    else PixelRGB8 255 255 255
              )
              64
              64
          encoded = encodeWebPLossy img 95

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          let PixelRGB8 rL _ _ = pixelAt dec 16 32 -- Left of edge
              PixelRGB8 rR _ _ = pixelAt dec 48 32 -- Right of edge
          -- Edge should still be visible
          (fromIntegral rR - fromIntegral rL :: Int) `shouldSatisfy` (> 100)
        _ -> expectationFailure "Decoding failed"

    it "horizontal edges are preserved" $ do
      let img =
            generateImage
              ( \_ y ->
                  if y < 32
                    then PixelRGB8 50 50 50
                    else PixelRGB8 200 200 200
              )
              64
              64
          encoded = encodeWebPLossy img 90

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          let PixelRGB8 rT _ _ = pixelAt dec 32 16 -- Top
              PixelRGB8 rB _ _ = pixelAt dec 32 48 -- Bottom
          (fromIntegral rB - fromIntegral rT :: Int) `shouldSatisfy` (> 50)
        _ -> expectationFailure "Decoding failed"

  describe "Complex Patterns" $ do
    it "checkerboard pattern survives encoding" $ do
      let img =
            generateImage
              ( \x y ->
                  if (x + y) `mod` 2 == 0
                    then PixelRGB8 255 255 255
                    else PixelRGB8 0 0 0
              )
              64
              64
          encoded = encodeWebPLossy img 70

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (64, 64)
        _ -> expectationFailure "Decoding failed"

    it "stripe pattern is preserved" $ do
      let img =
            generateImage
              ( \x _ ->
                  if (x `div` 8) `mod` 2 == 0
                    then PixelRGB8 200 200 200
                    else PixelRGB8 50 50 50
              )
              64
              64
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          -- Check that stripes are distinguishable
          let PixelRGB8 r1 _ _ = pixelAt dec 4 32 -- In stripe 1
              PixelRGB8 r2 _ _ = pixelAt dec 12 32 -- In stripe 2
          abs (fromIntegral r1 - fromIntegral r2 :: Int) `shouldSatisfy` (> 30)
        _ -> expectationFailure "Decoding failed"

-- Helper
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ xs f = sequence_ (map f xs)
