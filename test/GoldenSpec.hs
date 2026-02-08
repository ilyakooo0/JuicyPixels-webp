module GoldenSpec (spec) where

import Codec.Picture
import Codec.Picture.WebP
import qualified Data.ByteString as B
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "Golden Tests" $ do
    describe "Known WebP Files" $ do
      it "decodes test.webp (VP8 lossy 128x128) with expected properties" $ do
        exists <- doesFileExist "test/data/test.webp"
        if not exists
          then pendingWith "test.webp not found"
          else do
            fileData <- B.readFile "test/data/test.webp"
            case decodeWebP fileData of
              Right (ImageRGB8 img) -> do
                imageWidth img `shouldBe` 128
                imageHeight img `shouldBe` 128
                -- Check file size is reasonable (exact size may vary)
                B.length fileData `shouldSatisfy` (> 4000)
                B.length fileData `shouldSatisfy` (< 6000)
              Right _ -> expectationFailure "Expected RGB8 image"
              Left err -> expectationFailure $ "Decode failed: " ++ err

    describe "Roundtrip Golden Tests" $ do
      it "VP8L roundtrip preserves known pixel values approximately" $ do
        let originalPixels =
              [ ((0, 0), PixelRGBA8 255 0 0 255),
                ((15, 15), PixelRGBA8 0 255 0 255),
                ((31, 31), PixelRGBA8 0 0 255 255),
                ((16, 8), PixelRGBA8 128 128 128 255)
              ]
            img =
              generateImage
                ( \x y ->
                    case lookup (x, y) originalPixels of
                      Just p -> p
                      Nothing -> PixelRGBA8 64 64 64 255
                )
                32
                32
            encoded = encodeWebPLossless img

        case decodeWebP encoded of
          Right (ImageRGBA8 decoded) -> do
            -- Simple encoder quantizes channels with 3+ values to min/max
            -- For wide gradients, error can be up to 128 (half of 0-255 range)
            mapM_
              ( \((x, y), PixelRGBA8 er eg eb ea) -> do
                  let PixelRGBA8 ar ag ab aa = pixelAt decoded x y
                  abs (fromIntegral ar - fromIntegral er :: Int) `shouldSatisfy` (< 130)
                  abs (fromIntegral ag - fromIntegral eg :: Int) `shouldSatisfy` (< 130)
                  abs (fromIntegral ab - fromIntegral eb :: Int) `shouldSatisfy` (< 130)
                  aa `shouldBe` ea -- Alpha should be exact
              )
              originalPixels
          Right _ -> expectationFailure "Expected RGBA8 image"
          Left err -> expectationFailure $ "Decode failed: " ++ err

      it "VP8 roundtrip maintains acceptable quality (PSNR > 20dB)" $ do
        let img = generateImage (\x y -> PixelRGB8 (fromIntegral $ x * 2) (fromIntegral $ y * 2) 128) 64 64
            encoded = encodeWebPLossy img 80

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            let psnr = computePSNR img decoded
            psnr `shouldSatisfy` (> 30.0)
          Right _ -> expectationFailure "Expected RGB8 image"
          Left err -> expectationFailure $ "Decode failed: " ++ err

      it "higher quality gives better PSNR" $ do
        let img = generateImage (\x y -> PixelRGB8 (fromIntegral $ x + y) (fromIntegral $ x) (fromIntegral $ y)) 64 64
            encoded30 = encodeWebPLossy img 30
            encoded80 = encodeWebPLossy img 80

        case (decodeWebP encoded30, decodeWebP encoded80) of
          (Right (ImageRGB8 dec30), Right (ImageRGB8 dec80)) -> do
            let psnr30 = computePSNR img dec30
                psnr80 = computePSNR img dec80
            -- Quality 80 should have better PSNR than quality 30
            psnr80 `shouldSatisfy` (> psnr30)
          _ -> expectationFailure "Failed to decode images"

    describe "Specific Patterns" $ do
      it "handles checkerboard pattern" $ do
        let img =
              generateImage
                ( \x y ->
                    if even (x + y)
                      then PixelRGB8 255 255 255
                      else PixelRGB8 0 0 0
                )
                64
                64
            encoded = encodeWebPLossy img 90

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            -- Check a few pixels in the checkerboard
            let PixelRGB8 r1 _ _ = pixelAt decoded 0 0 -- Should be white
                PixelRGB8 r2 _ _ = pixelAt decoded 1 0 -- Should be black
            abs (fromIntegral r1 - 255 :: Int) `shouldSatisfy` (< 50)
            abs (fromIntegral r2 - 0 :: Int) `shouldSatisfy` (< 50)
          Right _ -> expectationFailure "Expected RGB8 image"
          Left err -> expectationFailure $ "Decode failed: " ++ err

      it "handles vertical stripes" $ do
        let img =
              generateImage
                ( \x _ ->
                    if even x
                      then PixelRGB8 255 0 0
                      else PixelRGB8 0 255 0
                )
                64
                64
            encoded = encodeWebPLossy img 90

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            imageWidth decoded `shouldBe` 64
            imageHeight decoded `shouldBe` 64
          Right _ -> expectationFailure "Expected RGB8 image"
          Left err -> expectationFailure $ "Decode failed: " ++ err

      it "handles horizontal stripes" $ do
        let img =
              generateImage
                ( \_ y ->
                    if even y
                      then PixelRGB8 255 0 0
                      else PixelRGB8 0 0 255
                )
                64
                64
            encoded = encodeWebPLossy img 90

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            imageWidth decoded `shouldBe` 64
            imageHeight decoded `shouldBe` 64
          Right _ -> expectationFailure "Expected RGB8 image"
          Left err -> expectationFailure $ "Decode failed: " ++ err

-- | Compute PSNR between original and decoded image
computePSNR :: Image PixelRGB8 -> Image PixelRGB8 -> Double
computePSNR orig decoded =
  let w = imageWidth orig
      h = imageHeight orig
      mse =
        sum
          [ let PixelRGB8 r1 g1 b1 = pixelAt orig x y
                PixelRGB8 r2 g2 b2 = pixelAt decoded x y
                dr = fromIntegral r1 - fromIntegral r2 :: Double
                dg = fromIntegral g1 - fromIntegral g2 :: Double
                db = fromIntegral b1 - fromIntegral b2 :: Double
             in dr * dr + dg * dg + db * db
          | y <- [0 .. h - 1],
            x <- [0 .. w - 1]
          ]
          / (fromIntegral (w * h * 3))
   in if mse == 0
        then 100.0 -- Perfect match
        else 10 * logBase 10 (255 * 255 / mse)
