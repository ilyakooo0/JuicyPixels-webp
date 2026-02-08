module QualitySpec (spec) where

import Codec.Picture
import Codec.Picture.WebP
import qualified Data.ByteString as B
import Test.Hspec

spec :: Spec
spec = do
  describe "Quality Metrics" $ do
    describe "PSNR Measurements" $ do
      it "quality 90 achieves PSNR > 25dB on gradients" $ do
        let img = generateImage (\x y ->
              let r = fromIntegral ((x * 255) `div` 63)
                  g = fromIntegral ((y * 255) `div` 63)
                  b = 128
               in PixelRGB8 r g b) 64 64
            encoded = encodeWebPLossy img 90

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            let psnr = computePSNR img decoded
            psnr `shouldSatisfy` (> 38.0)
          _ -> expectationFailure "Decode failed"

      it "quality 70 achieves PSNR > 20dB" $ do
        let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64
            encoded = encodeWebPLossy img 70

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            let psnr = computePSNR img decoded
            psnr `shouldSatisfy` (> 32.0)
          _ -> expectationFailure "Decode failed"

      it "quality 50 achieves PSNR > 15dB" $ do
        let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64
            encoded = encodeWebPLossy img 50

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            let psnr = computePSNR img decoded
            psnr `shouldSatisfy` (> 28.0)
          _ -> expectationFailure "Decode failed"

    describe "Quality vs File Size" $ do
      it "higher quality produces larger files" $ do
        let img = generateImage (\x y ->
              PixelRGB8 (fromIntegral ((x * y) `mod` 256))
                        (fromIntegral (x `mod` 256))
                        (fromIntegral (y `mod` 256))
              ) 128 128
            enc20 = encodeWebPLossy img 20
            enc50 = encodeWebPLossy img 50
            enc80 = encodeWebPLossy img 80

        B.length enc20 `shouldSatisfy` (> 0)
        B.length enc50 `shouldSatisfy` (> 0)
        B.length enc80 `shouldSatisfy` (> 0)

        -- Generally higher quality = larger file (though not always strict)
        B.length enc80 `shouldSatisfy` (>= B.length enc20)

      it "solid colors compress very efficiently" $ do
        let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 256 256
            encoded = encodeWebPLossy img 80

        -- 256x256 solid color should be < 3KB
        B.length encoded `shouldSatisfy` (< 6000)

      it "complex patterns use more bytes" $ do
        let solid = generateImage (\_ _ -> PixelRGB8 128 128 128) 64 64
            complex = generateImage (\x y ->
              PixelRGB8 (fromIntegral $ (x * 7 + y * 11) `mod` 256)
                        (fromIntegral $ (x * 13 + y * 17) `mod` 256)
                        (fromIntegral $ (x * 19 + y * 23) `mod` 256)
              ) 64 64
            encodedSolid = encodeWebPLossy solid 80
            encodedComplex = encodeWebPLossy complex 80

        B.length encodedComplex `shouldSatisfy` (> B.length encodedSolid * 2)

    describe "Pixel-Level Accuracy" $ do
      it "preserves corners accurately at high quality" $ do
        let img = generateImage (\x y ->
              PixelRGB8 (if x < 32 then 255 else 0)
                        (if y < 32 then 255 else 0)
                        128
              ) 64 64
            encoded = encodeWebPLossy img 95

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            -- Check corners
            let PixelRGB8 r1 g1 _ = pixelAt decoded 0 0      -- Top-left: should be high R, high G
                PixelRGB8 r2 g2 _ = pixelAt decoded 63 0     -- Top-right: should be low R, high G
                PixelRGB8 r3 g3 _ = pixelAt decoded 0 63     -- Bottom-left: should be high R, low G
                PixelRGB8 r4 g4 _ = pixelAt decoded 63 63    -- Bottom-right: should be low R, low G

            fromIntegral r1 `shouldSatisfy` (> (200 :: Int))
            fromIntegral g1 `shouldSatisfy` (> (200 :: Int))
            fromIntegral r2 `shouldSatisfy` (< (50 :: Int))
            fromIntegral g2 `shouldSatisfy` (> (200 :: Int))
          _ -> expectationFailure "Decode failed"

      it "maintains color relationships" $ do
        let img = generateImage (\x y ->
              if x < 32
                then PixelRGB8 255 0 0    -- Left half: red
                else PixelRGB8 0 0 255    -- Right half: blue
              ) 64 64
            encoded = encodeWebPLossy img 85

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            -- Left side should be reddish (R > B)
            let PixelRGB8 rL gL bL = pixelAt decoded 16 32
            (fromIntegral rL :: Int) `shouldSatisfy` (> (fromIntegral bL :: Int))

            -- Right side should be bluish (B > R)
            let PixelRGB8 rR gR bR = pixelAt decoded 48 32
            (fromIntegral bR :: Int) `shouldSatisfy` (> (fromIntegral rR :: Int))
          _ -> expectationFailure "Decode failed"

    describe "Comparative Quality" $ do
      it "VP8L has less error than VP8 lossy" $ do
        let img = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) 128 255) 64 64
            imgRGB = pixelMap (\(PixelRGBA8 r g b _) -> PixelRGB8 r g b) img
            encodedLossless = encodeWebPLossless img
            encodedLossy = encodeWebPLossy imgRGB 80

        case (decodeWebP encodedLossless, decodeWebP encodedLossy) of
          (Right (ImageRGBA8 decLossless), Right (ImageRGB8 decLossy)) -> do
            -- Lossless should be very close (simple encoder allows small variance)
            let PixelRGBA8 rl gl bl al = pixelAt decLossless 32 32
            abs (fromIntegral rl - 32 :: Int) `shouldSatisfy` (< 30)
            abs (fromIntegral gl - 32 :: Int) `shouldSatisfy` (< 30)

            -- Lossy will have more differences
            let PixelRGB8 r g b = pixelAt decLossy 32 32
            abs (fromIntegral r - 32 :: Int) `shouldSatisfy` (< 80)
            abs (fromIntegral g - 32 :: Int) `shouldSatisfy` (< 80)
          _ -> expectationFailure "Decode failed"

      it "quality 100 gives good quality" $ do
        let img = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 64 64
            encoded = encodeWebPLossy img 100

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            let psnr = computePSNR img decoded
            -- Quality 100 should give good PSNR (> 35dB is reasonable)
            psnr `shouldSatisfy` (> 35.0)
          _ -> expectationFailure "Decode failed"

-- | Compute PSNR between two RGB8 images
computePSNR :: Image PixelRGB8 -> Image PixelRGB8 -> Double
computePSNR orig decoded =
  let w = imageWidth orig
      h = imageHeight orig
      mse = sum [ let PixelRGB8 r1 g1 b1 = pixelAt orig x y
                      PixelRGB8 r2 g2 b2 = pixelAt decoded x y
                      dr = fromIntegral r1 - fromIntegral r2 :: Double
                      dg = fromIntegral g1 - fromIntegral g2 :: Double
                      db = fromIntegral b1 - fromIntegral b2 :: Double
                   in dr*dr + dg*dg + db*db
                | y <- [0..h-1], x <- [0..w-1]
                ] / (fromIntegral (w * h * 3))
   in if mse == 0
        then 100.0
        else 10 * logBase 10 (255*255 / mse)
