{-# LANGUAGE OverloadedStrings #-}

module StressSpec (spec) where

import Codec.Picture
import Codec.Picture.WebP
import qualified Data.ByteString as B
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Stress Tests" $ do
  describe "Large Images" $ do
    it "encodes and decodes 256x256 lossy" $ do
      let img = generateImage (\x y -> PixelRGB8 (fromIntegral $ x `mod` 256) (fromIntegral $ y `mod` 256) 128) 256 256
          encoded = encodeWebPLossy img 70

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          imageWidth dec `shouldBe` 256
          imageHeight dec `shouldBe` 256
        Right _ -> expectationFailure "Expected RGB8 image"
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "encodes and decodes 512x512 lossy" $ do
      let img = generateImage (\x y -> PixelRGB8 (fromIntegral $ (x * y) `mod` 256) 128 128) 512 512
          encoded = encodeWebPLossy img 50

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          imageWidth dec `shouldBe` 512
          imageHeight dec `shouldBe` 512
        Right _ -> expectationFailure "Expected RGB8 image"
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "encodes and decodes 256x256 lossless" $ do
      let img = generateImage (\x y -> PixelRGBA8 (fromIntegral $ x `mod` 256) (fromIntegral $ y `mod` 256) 128 255) 256 256
          encoded = encodeWebPLossless img

      case decodeWebP encoded of
        Right (ImageRGBA8 dec) -> do
          imageWidth dec `shouldBe` 256
          imageHeight dec `shouldBe` 256
        Right _ -> expectationFailure "Expected RGBA8 image"
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "encodes and decodes wide image 1024x64" $ do
      let img = generateImage (\x _ -> PixelRGB8 (fromIntegral $ x `mod` 256) 128 128) 1024 64
          encoded = encodeWebPLossy img 60

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          imageWidth dec `shouldBe` 1024
          imageHeight dec `shouldBe` 64
        Right _ -> expectationFailure "Expected RGB8 image"
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "encodes and decodes tall image 64x1024" $ do
      let img = generateImage (\_ y -> PixelRGB8 128 (fromIntegral $ y `mod` 256) 128) 64 1024
          encoded = encodeWebPLossy img 60

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> do
          imageWidth dec `shouldBe` 64
          imageHeight dec `shouldBe` 1024
        Right _ -> expectationFailure "Expected RGB8 image"
        Left err -> expectationFailure $ "Decode failed: " ++ err

  describe "Unusual Dimensions" $ do
    it "handles prime dimensions (127x131)" $ do
      let img = generateImage (\_ _ -> PixelRGB8 100 150 200) 127 131
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (127, 131)
        _ -> expectationFailure "Decode failed"

    it "handles power of 2 minus 1 (255x255)" $ do
      let img = generateImage (\_ _ -> PixelRGB8 80 120 160) 255 255
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (255, 255)
        _ -> expectationFailure "Decode failed"

    it "handles power of 2 plus 1 (257x257)" $ do
      let img = generateImage (\_ _ -> PixelRGB8 50 100 150) 257 257
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (257, 257)
        _ -> expectationFailure "Decode failed"

    it "handles highly asymmetric (1x512)" $ do
      let img = generateImage (\_ y -> PixelRGB8 128 (fromIntegral $ y `mod` 256) 128) 1 512
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (1, 512)
        _ -> expectationFailure "Decode failed"

    it "handles highly asymmetric (512x1)" $ do
      let img = generateImage (\x _ -> PixelRGB8 (fromIntegral $ x `mod` 256) 128 128) 512 1
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (512, 1)
        _ -> expectationFailure "Decode failed"

  describe "Complex Patterns" $ do
    it "handles mandelbrot-like pattern" $ do
      let img =
            generateImage
              ( \x y ->
                  let cx = (fromIntegral x - 64) / 32 :: Double
                      cy = (fromIntegral y - 64) / 32 :: Double
                      iter = mandelbrotIter cx cy 100
                   in PixelRGB8 (fromIntegral iter) (fromIntegral $ iter * 2 `mod` 256) (fromIntegral $ iter * 3 `mod` 256)
              )
              128
              128
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (128, 128)
        _ -> expectationFailure "Decode failed"

    it "handles noise pattern" $ do
      let img =
            generateImage
              ( \x y ->
                  let v = (x * 17 + y * 31 + x * y * 7) `mod` 256
                   in PixelRGB8 (fromIntegral v) (fromIntegral $ (v + 85) `mod` 256) (fromIntegral $ (v + 170) `mod` 256)
              )
              128
              128
          encoded = encodeWebPLossy img 70

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (128, 128)
        _ -> expectationFailure "Decode failed"

    it "handles radial gradient" $ do
      let img =
            generateImage
              ( \x y ->
                  let dx = fromIntegral (x - 64) :: Double
                      dy = fromIntegral (y - 64) :: Double
                      dist = sqrt (dx * dx + dy * dy)
                      v = min 255 (floor $ dist * 3)
                   in PixelRGB8 v v v
              )
              128
              128
          encoded = encodeWebPLossy img 80

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (128, 128)
        _ -> expectationFailure "Decode failed"

    it "handles diagonal stripes" $ do
      let img =
            generateImage
              ( \x y ->
                  if (x + y) `mod` 16 < 8
                    then PixelRGB8 255 255 255
                    else PixelRGB8 0 0 0
              )
              128
              128
          encoded = encodeWebPLossy img 90

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (128, 128)
        _ -> expectationFailure "Decode failed"

  describe "Alpha Channel Stress" $ do
    it "handles varying alpha across image" $ do
      let img = generateImage (\x y -> PixelRGBA8 255 0 0 (fromIntegral $ (x + y) `mod` 256)) 64 64
          encoded = encodeWebPLossyWithAlpha img 80

      case decodeWebP encoded of
        Right (ImageRGBA8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (64, 64)
        _ -> expectationFailure "Decode failed"

    it "handles mostly transparent image" $ do
      let img = generateImage (\_ _ -> PixelRGBA8 255 255 255 10) 64 64
          encoded = encodeWebPLossyWithAlpha img 80

      case decodeWebP encoded of
        Right (ImageRGBA8 dec) -> do
          let PixelRGBA8 _ _ _ a = pixelAt dec 32 32
          a `shouldSatisfy` (< 20) -- Low alpha
        _ -> expectationFailure "Decode failed"

    it "handles alpha gradient" $ do
      let img =
            generateImage
              ( \x _ ->
                  PixelRGBA8 128 128 255 (fromIntegral $ (x * 4) `min` 255)
              )
              64
              64
          encoded = encodeWebPLossyWithAlpha img 80

      case decodeWebP encoded of
        Right (ImageRGBA8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (64, 64)
        _ -> expectationFailure "Decode failed"

  describe "Quality Extremes" $ do
    it "quality 1 produces decodable output" $ do
      let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64
          encoded = encodeWebPLossy img 1

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (64, 64)
        _ -> expectationFailure "Decode failed"

    it "quality 99 produces decodable output" $ do
      let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64
          encoded = encodeWebPLossy img 99

      case decodeWebP encoded of
        Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (64, 64)
        _ -> expectationFailure "Decode failed"

    it "all quality levels from 0 to 100 produce valid output" $ do
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 32 32
          qualities = [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

      forM_ qualities $ \q -> do
        let encoded = encodeWebPLossy img q
        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (32, 32)
          Right _ -> expectationFailure $ "Wrong format for quality " ++ show q
          Left err -> expectationFailure $ "Quality " ++ show q ++ " failed: " ++ err

  describe "Repeated Operations" $ do
    it "multiple encode-decode cycles preserve dimensions" $ do
      let original = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64

      let cycle1 = case decodeWebP (encodeWebPLossy original 80) of
            Right (ImageRGB8 img) -> img
            _ -> error "Cycle 1 failed"

      let cycle2 = case decodeWebP (encodeWebPLossy cycle1 80) of
            Right (ImageRGB8 img) -> img
            _ -> error "Cycle 2 failed"

      let cycle3 = case decodeWebP (encodeWebPLossy cycle2 80) of
            Right (ImageRGB8 img) -> img
            _ -> error "Cycle 3 failed"

      (imageWidth cycle3, imageHeight cycle3) `shouldBe` (64, 64)

    it "lossless multiple cycles preserve data exactly" $ do
      let original = generateImage (\_ _ -> PixelRGBA8 100 150 200 255) 32 32

      let cycle1 = case decodeWebP (encodeWebPLossless original) of
            Right (ImageRGBA8 img) -> img
            _ -> error "Cycle 1 failed"

      let cycle2 = case decodeWebP (encodeWebPLossless cycle1) of
            Right (ImageRGBA8 img) -> img
            _ -> error "Cycle 2 failed"

      -- For solid color, should be pixel-perfect
      let PixelRGBA8 r g b a = pixelAt cycle2 16 16
      (r, g, b, a) `shouldBe` (100, 150, 200, 255)

  describe "File Size Patterns" $ do
    it "complex images are larger than simple ones" $ do
      let simple = generateImage (\_ _ -> PixelRGB8 128 128 128) 128 128
          complex =
            generateImage
              ( \x y ->
                  let v = (x * 17 + y * 31) `mod` 256
                   in PixelRGB8 (fromIntegral v) (fromIntegral $ (v + 85) `mod` 256) (fromIntegral $ (v + 170) `mod` 256)
              )
              128
              128
          simpleEncoded = encodeWebPLossy simple 80
          complexEncoded = encodeWebPLossy complex 80

      B.length complexEncoded `shouldSatisfy` (> B.length simpleEncoded)

    it "higher quality produces larger files" $ do
      let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64
          lowQ = encodeWebPLossy img 20
          highQ = encodeWebPLossy img 95

      B.length highQ `shouldSatisfy` (>= B.length lowQ)

    it "larger images produce larger files" $ do
      let small = generateImage (\_ _ -> PixelRGB8 128 128 128) 32 32
          large = generateImage (\_ _ -> PixelRGB8 128 128 128) 256 256
          smallEncoded = encodeWebPLossy small 80
          largeEncoded = encodeWebPLossy large 80

      B.length largeEncoded `shouldSatisfy` (> B.length smallEncoded)

-- Helper: Mandelbrot iteration count
mandelbrotIter :: Double -> Double -> Int -> Int
mandelbrotIter cx cy maxIter = go 0 0 0
  where
    go x y i
      | i >= maxIter = maxIter
      | x * x + y * y > 4 = i
      | otherwise = go (x * x - y * y + cx) (2 * x * y + cy) (i + 1)

-- forM_ for testing
forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ xs f = sequence_ (map f xs)
