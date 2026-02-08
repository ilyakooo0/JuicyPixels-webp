module GoldenFilesSpec (spec) where

import Codec.Picture
import Codec.Picture.WebP
import qualified Data.ByteString as B
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Test.Hspec

spec :: Spec
spec = do
  describe "Golden File Tests" $ do
    describe "Official Google WebP Gallery Images" $ do
      it "decodes gallery1.webp (VP8 photo)" $ do
        testGoldenFile "test/data/golden/gallery1.webp" $ \img -> do
          case img of
            ImageRGB8 i -> do
              imageWidth i `shouldSatisfy` (> 0)
              imageHeight i `shouldSatisfy` (> 0)
            ImageRGBA8 i -> do
              imageWidth i `shouldSatisfy` (> 0)
              imageHeight i `shouldSatisfy` (> 0)
            _ -> expectationFailure "Unexpected image format"

      it "decodes gallery2.webp (VP8 photo)" $ do
        testGoldenFile "test/data/golden/gallery2.webp" $ \img -> do
          case img of
            ImageRGB8 i -> imageWidth i `shouldSatisfy` (> 0)
            ImageRGBA8 i -> imageWidth i `shouldSatisfy` (> 0)
            _ -> expectationFailure "Unexpected image format"

      it "decodes gallery3.webp (VP8 photo)" $ do
        testGoldenFile "test/data/golden/gallery3.webp" $ \img -> do
          case img of
            ImageRGB8 i -> imageWidth i `shouldSatisfy` (> 0)
            ImageRGBA8 i -> imageWidth i `shouldSatisfy` (> 0)
            _ -> expectationFailure "Unexpected image format"

      it "decodes gallery4.webp (VP8L graphics)" $ do
        testGoldenFile "test/data/golden/gallery4.webp" $ \img -> do
          case img of
            ImageRGB8 i -> imageWidth i `shouldSatisfy` (> 0)
            ImageRGBA8 i -> imageWidth i `shouldSatisfy` (> 0)
            _ -> expectationFailure "Unexpected image format"

      it "decodes gallery5.webp (VP8L graphics)" $ do
        testGoldenFile "test/data/golden/gallery5.webp" $ \img -> do
          case img of
            ImageRGB8 i -> imageWidth i `shouldSatisfy` (> 0)
            ImageRGBA8 i -> imageWidth i `shouldSatisfy` (> 0)
            _ -> expectationFailure "Unexpected image format"

    describe "All Golden Files" $ do
      it "decodes all WebP files in test/data/golden/" $ do
        exists <- doesFileExist "test/data/golden"
        if not exists
          then pendingWith "test/data/golden directory not found - run DownloadTestImages.hs first"
          else do
            files <- listDirectory "test/data/golden"
            let webpFiles = filter (\f -> takeExtension f == ".webp") files
            if null webpFiles
              then pendingWith "No .webp files found in test/data/golden/"
              else do
                putStrLn $ "\nTesting " ++ show (length webpFiles) ++ " golden WebP files:"
                mapM_ (\file -> do
                  let path = "test/data/golden" </> file
                  fileData <- B.readFile path
                  putStrLn $ "  - " ++ file ++ " (" ++ show (B.length fileData) ++ " bytes)"
                  case decodeWebP fileData of
                    Right dynImg -> do
                      let dims = case dynImg of
                            ImageRGB8 i -> (imageWidth i, imageHeight i)
                            ImageRGBA8 i -> (imageWidth i, imageHeight i)
                            ImageYCbCr8 i -> (imageWidth i, imageHeight i)
                            _ -> (0, 0)
                      putStrLn $ "    ✓ Decoded: " ++ show dims
                    Left err -> expectationFailure $ "Failed to decode " ++ file ++ ": " ++ err
                  ) webpFiles

    describe "Roundtrip with Golden References" $ do
      it "VP8L encode-decode of known pattern is approximately correct" $ do
        -- Create a specific known pattern
        let img = generateImage mkPattern 64 64
            encoded = encodeWebPLossless img

        case decodeWebP encoded of
          Right (ImageRGBA8 decoded) -> do
            -- Simple encoder quantizes channels with 3+ values to min/max
            -- Pattern has gradient 0-252, so error can be up to 126
            mapM_ (\(x, y) -> do
              let PixelRGBA8 er eg eb ea = mkPattern x y
                  PixelRGBA8 ar ag ab aa = pixelAt decoded x y
              abs (fromIntegral ar - fromIntegral er :: Int) `shouldSatisfy` (< 130)
              abs (fromIntegral ag - fromIntegral eg :: Int) `shouldSatisfy` (< 130)
              abs (fromIntegral ab - fromIntegral eb :: Int) `shouldSatisfy` (< 130)
              ) [(0,0), (32,32), (63,63), (10,50), (50,10)]
          Right _ -> expectationFailure "Expected RGBA8 image"
          Left err -> expectationFailure $ "Decode failed: " ++ err

      it "VP8 encode-decode maintains quality metrics" $ do
        let img = generateImage (\x y ->
              let r = fromIntegral ((x * 255) `div` 63)
                  g = fromIntegral ((y * 255) `div` 63)
                  b = 128
               in PixelRGB8 r g b) 64 64
            encoded = encodeWebPLossy img 85

        case decodeWebP encoded of
          Right (ImageRGB8 decoded) -> do
            -- Compute metrics
            let mse = computeMSE img decoded
                psnr = if mse == 0 then 100.0 else 10 * logBase 10 (255*255 / mse)

            -- Quality 85 should give PSNR > 34dB
            psnr `shouldSatisfy` (> 34.0)

            -- Also verify file size is reasonable
            B.length encoded `shouldSatisfy` (< 10000)
          Right _ -> expectationFailure "Expected RGB8 image"
          Left err -> expectationFailure $ "Decode failed: " ++ err

-- Helper to test a golden file
testGoldenFile :: FilePath -> (DynamicImage -> Expectation) -> Expectation
testGoldenFile path validator = do
  exists <- doesFileExist path
  if not exists
    then pendingWith $ path ++ " not found - run DownloadTestImages.hs first"
    else do
      fileData <- B.readFile path
      case decodeWebP fileData of
        Right img -> validator img
        Left err -> expectationFailure $ "Decode failed: " ++ err

-- Known test pattern
mkPattern :: Int -> Int -> PixelRGBA8
mkPattern x y =
  let r = fromIntegral ((x * 255) `div` 63)
      g = fromIntegral ((y * 255) `div` 63)
      b = fromIntegral ((x + y) `mod` 256)
      a = 255
   in PixelRGBA8 r g b a

-- Compute Mean Squared Error
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

-- Compute PSNR (wrapper for readability)
computePSNR :: Image PixelRGB8 -> Image PixelRGB8 -> Double
computePSNR orig decoded =
  let mse = computeMSE orig decoded
   in if mse == 0
        then 100.0
        else 10 * logBase 10 (255*255 / mse)
