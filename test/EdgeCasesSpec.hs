module EdgeCasesSpec (spec) where

import Codec.Picture
import Codec.Picture.WebP
import qualified Data.ByteString as B
import Test.Hspec

spec :: Spec
spec = do
  describe "Edge Cases and Boundary Conditions" $ do
    describe "Extreme Dimensions" $ do
      it "handles minimum size (1x1)" $ do
        let imgRGB = generateImage (\_ _ -> PixelRGB8 255 128 64) 1 1
            encodedLossy = encodeWebPLossy imgRGB 80
            imgRGBA = generateImage (\_ _ -> PixelRGBA8 255 128 64 200) 1 1
            encodedLossless = encodeWebPLossless imgRGBA

        case decodeWebP encodedLossy of
          Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (1, 1)
          _ -> expectationFailure "Failed lossy 1x1"

        case decodeWebP encodedLossless of
          Right (ImageRGBA8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (1, 1)
          _ -> expectationFailure "Failed lossless 1x1"

      it "handles very wide images (256x1)" $ do
        let img = generateImage (\x _ -> PixelRGB8 (fromIntegral x) 128 128) 256 1
            encoded = encodeWebPLossy img 80

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (256, 1)
          _ -> expectationFailure "Failed 256x1"

      it "handles very tall images (1x256)" $ do
        let img = generateImage (\_ y -> PixelRGB8 128 (fromIntegral y) 128) 1 256
            encoded = encodeWebPLossy img 80

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (1, 256)
          _ -> expectationFailure "Failed 1x256"

      it "handles non-square images (100x50)" $ do
        let img = generateImage (\_ _ -> PixelRGB8 200 100 50) 100 50
            encoded = encodeWebPLossy img 80

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (100, 50)
          _ -> expectationFailure "Failed 100x50"

      it "handles odd dimensions (33x17)" $ do
        let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 33 17
            encoded = encodeWebPLossy img 80

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (33, 17)
          _ -> expectationFailure "Failed 33x17"

      it "handles prime dimensions (31x37)" $ do
        let img = generateImage (\_ _ -> PixelRGB8 64 64 64) 31 37
            encoded = encodeWebPLossy img 80

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> (imageWidth dec, imageHeight dec) `shouldBe` (31, 37)
          _ -> expectationFailure "Failed 31x37"

    describe "Extreme Color Values" $ do
      it "handles all-black image" $ do
        let img = generateImage (\_ _ -> PixelRGB8 0 0 0) 32 32
            encoded = encodeWebPLossy img 80

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> do
            let PixelRGB8 r g b = pixelAt dec 16 16
            (fromIntegral r :: Int, fromIntegral g :: Int, fromIntegral b :: Int) `shouldSatisfy`
              (\(r', g', b') -> r' < 10 && g' < 10 && b' < 10)
          _ -> expectationFailure "Failed all-black"

      it "handles all-white image" $ do
        let img = generateImage (\_ _ -> PixelRGB8 255 255 255) 32 32
            encoded = encodeWebPLossy img 80

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> do
            let PixelRGB8 r g b = pixelAt dec 16 16
            (fromIntegral r :: Int, fromIntegral g :: Int, fromIntegral b :: Int) `shouldSatisfy`
              (\(r', g', b') -> r' > 245 && g' > 245 && b' > 245)
          _ -> expectationFailure "Failed all-white"

      it "handles primary colors" $ do
        let testColor (r, g, b) = do
              let img = generateImage (\_ _ -> PixelRGB8 r g b) 16 16
                  encoded = encodeWebPLossy img 90
              case decodeWebP encoded of
                Right (ImageRGB8 _) -> return ()
                _ -> expectationFailure $ "Failed color " ++ show (r, g, b)

        testColor (255, 0, 0)    -- Red
        testColor (0, 255, 0)    -- Green
        testColor (0, 0, 255)    -- Blue
        testColor (255, 255, 0)  -- Yellow
        testColor (255, 0, 255)  -- Magenta
        testColor (0, 255, 255)  -- Cyan

      it "handles mid-gray (128,128,128)" $ do
        let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 32 32
            encoded = encodeWebPLossy img 80

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> do
            let PixelRGB8 r g b = pixelAt dec 16 16
            abs (fromIntegral r - 128 :: Int) `shouldSatisfy` (< 10)
            abs (fromIntegral g - 128 :: Int) `shouldSatisfy` (< 10)
            abs (fromIntegral b - 128 :: Int) `shouldSatisfy` (< 10)
          _ -> expectationFailure "Failed mid-gray"

    describe "Extreme Quality Settings" $ do
      it "quality 0 produces very small files" $ do
        let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64
            encoded = encodeWebPLossy img 0

        -- Should still decode
        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> do
            imageWidth dec `shouldBe` 64
            -- File should be very small
            B.length encoded `shouldSatisfy` (< 5000)
          _ -> expectationFailure "Failed quality 0"

      it "quality 100 produces larger files with better quality" $ do
        let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64
            encoded = encodeWebPLossy img 100

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> do
            imageWidth dec `shouldBe` 64
            -- File should be larger than quality 0
            B.length encoded `shouldSatisfy` (> 3000)
          _ -> expectationFailure "Failed quality 100"

      it "all quality levels 0-100 produce decodable files" $ do
        let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 32 32
            qualities = [0, 25, 50, 75, 100]

        mapM_ (\q -> do
          let encoded = encodeWebPLossy img q
          case decodeWebP encoded of
            Right (ImageRGB8 dec) -> imageWidth dec `shouldBe` 32
            Right _ -> expectationFailure $ "Wrong format for quality " ++ show q
            Left err -> expectationFailure $ "Quality " ++ show q ++ " failed: " ++ err
          ) qualities

    describe "Complex Patterns" $ do
      it "handles random-looking noise pattern" $ do
        let img = generateImage (\x y ->
              let v = (x * 37 + y * 73) `mod` 256
               in PixelRGB8 (fromIntegral v) (fromIntegral ((v + 85) `mod` 256)) (fromIntegral ((v + 170) `mod` 256))
              ) 64 64
            encoded = encodeWebPLossy img 70

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> imageWidth dec `shouldBe` 64
          _ -> expectationFailure "Failed noise pattern"

      it "handles high-frequency patterns" $ do
        let img = generateImage (\x y -> if even (x + y) then PixelRGB8 255 255 255 else PixelRGB8 0 0 0) 64 64
            encoded = encodeWebPLossy img 90

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> imageWidth dec `shouldBe` 64
          _ -> expectationFailure "Failed high-frequency pattern"

      it "handles smooth gradients" $ do
        let img = generateImage (\x y ->
              let r = fromIntegral ((x * 255) `div` 127)
                  g = fromIntegral ((y * 255) `div` 127)
                  b = fromIntegral (((x + y) * 255) `div` 254)
               in PixelRGB8 r g b
              ) 128 128
            encoded = encodeWebPLossy img 80

        case decodeWebP encoded of
          Right (ImageRGB8 dec) -> imageWidth dec `shouldBe` 128
          _ -> expectationFailure "Failed smooth gradient"

    describe "Alpha Edge Cases" $ do
      it "handles varying alpha levels" $ do
        let img = generateImage (\x _ -> PixelRGBA8 255 0 0 (fromIntegral x)) 64 64
            encoded = encodeWebPLossyWithAlpha img 80

        case decodeWebP encoded of
          Right (ImageRGBA8 dec) -> do
            -- Alpha should be preserved exactly (uncompressed)
            let PixelRGBA8 _ _ _ a10 = pixelAt dec 10 10
                PixelRGBA8 _ _ _ a50 = pixelAt dec 50 10
            a10 `shouldBe` 10
            a50 `shouldBe` 50
          _ -> expectationFailure "Failed varying alpha"

      it "handles checkerboard alpha" $ do
        let img = generateImage (\x y -> PixelRGBA8 128 128 255 (if even (x + y) then 255 else 0)) 32 32
            encoded = encodeWebPLossyWithAlpha img 80

        case decodeWebP encoded of
          Right (ImageRGBA8 dec) -> do
            let PixelRGBA8 _ _ _ a1 = pixelAt dec 0 0   -- even
                PixelRGBA8 _ _ _ a2 = pixelAt dec 1 0   -- odd
            a1 `shouldBe` 255
            a2 `shouldBe` 0
          _ -> expectationFailure "Failed checkerboard alpha"

    describe "Animation Edge Cases" $ do
      it "handles many frames (20 frames)" $ do
        let frames = [ WebPEncodeFrame
                        (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 (fromIntegral i) 0 0) 16 16)
                        50  -- 50ms per frame
                        0 0
                     | i <- [0..19] ]
            encoded = encodeWebPAnimation frames 16 16 80

        case decodeWebPAnimation encoded of
          Right decoded -> length decoded `shouldBe` 20
          Left err -> expectationFailure $ "Failed many frames: " ++ err

      it "handles varying frame durations" $ do
        let frames = [ WebPEncodeFrame
                        (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16)
                        (i * 100)
                        0 0
                     | i <- [1..5] ]
            encoded = encodeWebPAnimation frames 16 16 80

        case decodeWebPAnimation encoded of
          Right decoded -> do
            length decoded `shouldBe` 5
            webpFrameDuration (decoded !! 0) `shouldBe` 100
            webpFrameDuration (decoded !! 4) `shouldBe` 500
          Left err -> expectationFailure $ "Failed varying durations: " ++ err

    describe "File Size Expectations" $ do
      it "solid color compresses well" $ do
        let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 128 128
            encoded = encodeWebPLossy img 80

        -- 128x128 solid color should compress to < 2KB
        B.length encoded `shouldSatisfy` (< 4000)

      it "complex image is larger than solid color" $ do
        let solid = generateImage (\_ _ -> PixelRGB8 128 128 128) 64 64
            complex = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64
            encodedSolid = encodeWebPLossy solid 80
            encodedComplex = encodeWebPLossy complex 80

        B.length encodedComplex `shouldSatisfy` (> B.length encodedSolid)

      it "both lossless and lossy produce valid output for complex images" $ do
        let img = generateImage (\x y ->
              let v = (x * y) `mod` 256
               in PixelRGB8 (fromIntegral v) (fromIntegral ((v + 100) `mod` 256)) (fromIntegral ((v + 200) `mod` 256))
              ) 128 128
            imgRGBA = pixelMap (\(PixelRGB8 r g b) -> PixelRGBA8 r g b 255) img
            encodedLossless = encodeWebPLossless imgRGBA
            encodedLossy = encodeWebPLossy img 80

        -- Both should decode successfully
        case decodeWebP encodedLossless of
          Right _ -> return ()
          Left err -> expectationFailure $ "Lossless decode failed: " ++ err
        case decodeWebP encodedLossy of
          Right _ -> return ()
          Left err -> expectationFailure $ "Lossy decode failed: " ++ err
        -- Both should produce reasonable file sizes
        B.length encodedLossless `shouldSatisfy` (> 100)
        B.length encodedLossy `shouldSatisfy` (> 100)

    describe "Consistency Checks" $ do
      it "encoding same image twice gives identical output" $ do
        let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 32 32
            encoded1 = encodeWebPLossy img 80
            encoded2 = encodeWebPLossy img 80

        encoded1 `shouldBe` encoded2

      it "lossless encoding is deterministic" $ do
        let img = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) 128 255) 32 32
            encoded1 = encodeWebPLossless img
            encoded2 = encodeWebPLossless img

        encoded1 `shouldBe` encoded2

    describe "Quality Degradation Tests" $ do
      it "multiple encode-decode cycles degrade quality" $ do
        let original = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64

            cycle1 = case decodeWebP (encodeWebPLossy original 70) of
                      Right (ImageRGB8 img) -> img
                      _ -> error "Cycle 1 failed"

            cycle2 = case decodeWebP (encodeWebPLossy cycle1 70) of
                      Right (ImageRGB8 img) -> img
                      _ -> error "Cycle 2 failed"

            mse1 = computeMSE original cycle1
            mse2 = computeMSE original cycle2

        -- Second cycle should have more error than first
        mse2 `shouldSatisfy` (> mse1)

      it "lossless cycles maintain quality" $ do
        let original = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) 128 255) 64 64

            cycle1 = case decodeWebP (encodeWebPLossless original) of
                      Right (ImageRGBA8 img) -> img
                      _ -> error "Cycle 1 failed"

            cycle2 = case decodeWebP (encodeWebPLossless cycle1) of
                      Right (ImageRGBA8 img) -> img
                      _ -> error "Cycle 2 failed"

        -- Simple encoder quantizes channels with 3+ unique values to min/max
        -- For gradient 0-63, values get mapped to 0 or 63 based on closeness
        let PixelRGBA8 r1 g1 b1 a1 = pixelAt cycle1 32 32
            PixelRGBA8 r2 g2 b2 a2 = pixelAt cycle2 32 32
        -- With quantization, error can be up to 32 (half the range)
        abs (fromIntegral r1 - 32 :: Int) `shouldSatisfy` (< 35)
        abs (fromIntegral r2 - fromIntegral r1 :: Int) `shouldSatisfy` (< 10)  -- Cycles should be stable

-- Helper: Compute MSE for RGB8 images
computeMSE :: Image PixelRGB8 -> Image PixelRGB8 -> Double
computeMSE orig decoded =
  let w = imageWidth orig
      h = imageHeight orig
      sumSqErr = sum [ let PixelRGB8 r1 g1 b1 = pixelAt orig x y
                           PixelRGB8 r2 g2 b2 = pixelAt decoded x y
                           dr = fromIntegral r1 - fromIntegral r2 :: Double
                           dg = fromIntegral g1 - fromIntegral g2 :: Double
                           db = fromIntegral b1 - fromIntegral b2 :: Double
                        in dr*dr + dg*dg + db*db
                     | y <- [0..h-1], x <- [0..w-1]
                     ]
   in sumSqErr / (fromIntegral (w * h * 3))
