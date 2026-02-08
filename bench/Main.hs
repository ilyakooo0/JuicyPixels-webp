{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Comprehensive benchmarks for JuicyPixels-webp
module Main where

import Codec.Picture (DynamicImage (..), Image (..), PixelRGB8 (..), PixelRGBA8 (..), generateImage)
import Codec.Picture.Types
import Codec.Picture.WebP
import Codec.Picture.WebP.Internal.BitReader
import Codec.Picture.WebP.Internal.BitWriter
import Codec.Picture.WebP.Internal.Container
import Codec.Picture.WebP.Internal.VP8.BoolDecoder
import Codec.Picture.WebP.Internal.VP8.BoolEncoder
import Codec.Picture.WebP.Internal.VP8.DCT
import Codec.Picture.WebP.Internal.VP8.IDCT
import Control.DeepSeq
import Control.Monad (forM_, replicateM, when)
import Control.Monad.ST
import Criterion.Main
import Data.Bits
import qualified Data.ByteString as B
import Data.Int (Int16)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word
import System.Directory (doesFileExist)

-- NFData instance for WebPAnimFrame (DynamicImage already has NFData from JuicyPixels)
instance NFData WebPAnimFrame where
  rnf (WebPAnimFrame img dur x y) = rnf img `seq` rnf dur `seq` rnf x `seq` rnf y

main :: IO ()
main = do
  -- Load test images
  testFiles <- loadTestFiles

  defaultMain
    [ bgroup "decode" $ decodeEndToEndBenchmarks testFiles
    , bgroup "encode" encodeEndToEndBenchmarks
    , bgroup "components" componentBenchmarks
    , bgroup "roundtrip" roundtripBenchmarks
    ]

-- | Load test files for benchmarking
loadTestFiles :: IO [(String, B.ByteString)]
loadTestFiles = do
  let testPaths =
        [ ("small", "test/data/test_simple_vp8l.webp")
        , ("gallery1-30KB", "test/data/golden/gallery1.webp")
        , ("gallery2-60KB", "test/data/golden/gallery2.webp")
        , ("gallery3-199KB", "test/data/golden/gallery3.webp")
        , ("gallery4-173KB", "test/data/golden/gallery4.webp")
        , ("gallery5-81KB", "test/data/golden/gallery5.webp")
        ]

  files <- mapM loadIfExists testPaths
  return $ concat files
  where
    loadIfExists (name, path) = do
      exists <- doesFileExist path
      if exists
        then do
          contents <- B.readFile path
          return [(name, contents)]
        else return []

-- | End-to-end decoding benchmarks
decodeEndToEndBenchmarks :: [(String, B.ByteString)] -> [Benchmark]
decodeEndToEndBenchmarks testFiles =
  [ bgroup "decodeWebP" $
      map (\(name, bs) -> bench name $ nf decodeWebP bs) testFiles
  , bgroup "parseWebP" $
      map (\(name, bs) -> bench name $ whnf parseWebP bs) testFiles
  ]

-- | End-to-end encoding benchmarks
encodeEndToEndBenchmarks :: [Benchmark]
encodeEndToEndBenchmarks =
  [ bgroup "lossless" $
      [ bench "32x32" $ nf encodeWebPLossless (generateTestImage 32 32)
      , bench "64x64" $ nf encodeWebPLossless (generateTestImage 64 64)
      , bench "128x128" $ nf encodeWebPLossless (generateTestImage 128 128)
      , bench "256x256" $ nf encodeWebPLossless (generateTestImage 256 256)
      , bench "512x512" $ nf encodeWebPLossless (generateTestImage 512 512)
      ]
  , bgroup "lossless-patterns" $
      [ bench "solid-color" $ nf encodeWebPLossless (generateSolidImage 256 256 (PixelRGBA8 128 64 192 255))
      , bench "gradient" $ nf encodeWebPLossless (generateGradientImage 256 256)
      , bench "checkerboard" $ nf encodeWebPLossless (generateCheckerboard 256 256)
      , bench "random-noise" $ nf encodeWebPLossless (generateNoiseImage 256 256)
      ]
  , bgroup "lossy" $
      [ bgroup "quality-50" $
          [ bench "32x32" $ nf (encodeWebPLossy' 50) (generateTestImageRGB 32 32)
          , bench "64x64" $ nf (encodeWebPLossy' 50) (generateTestImageRGB 64 64)
          , bench "128x128" $ nf (encodeWebPLossy' 50) (generateTestImageRGB 128 128)
          , bench "256x256" $ nf (encodeWebPLossy' 50) (generateTestImageRGB 256 256)
          ]
      , bgroup "quality-sweep-128x128" $
          [ bench "q10" $ nf (encodeWebPLossy' 10) (generateTestImageRGB 128 128)
          , bench "q30" $ nf (encodeWebPLossy' 30) (generateTestImageRGB 128 128)
          , bench "q50" $ nf (encodeWebPLossy' 50) (generateTestImageRGB 128 128)
          , bench "q70" $ nf (encodeWebPLossy' 70) (generateTestImageRGB 128 128)
          , bench "q90" $ nf (encodeWebPLossy' 90) (generateTestImageRGB 128 128)
          ]
      ]
  , bgroup "lossy-with-alpha" $
      [ bench "128x128-q50" $ nf (encodeWebPLossyWithAlpha' 50) (generateTestImage 128 128)
      , bench "256x256-q50" $ nf (encodeWebPLossyWithAlpha' 50) (generateTestImage 256 256)
      ]
  ]
  where
    encodeWebPLossy' q img = encodeWebPLossy img q
    encodeWebPLossyWithAlpha' q img = encodeWebPLossyWithAlpha img q

-- | Component-level benchmarks
componentBenchmarks :: [Benchmark]
componentBenchmarks =
  [ bgroup "bitreader" bitReaderBenchmarks
  , bgroup "bitwriter" bitWriterBenchmarks
  , bgroup "dct" dctBenchmarks
  , bgroup "idct" idctBenchmarks
  , bgroup "color-convert" colorConvertBenchmarks
  , bgroup "loop-filter" loopFilterBenchmarks
  , bgroup "bool-decoder" boolDecoderBenchmarks
  , bgroup "bool-encoder" boolEncoderBenchmarks
  , bgroup "transform" transformBenchmarks
  ]

-- | BitReader benchmarks
bitReaderBenchmarks :: [Benchmark]
bitReaderBenchmarks =
  let testData = B.pack [0..255]
      reader = initBitReader testData
  in
  [ bench "init" $ whnf initBitReader testData
  , bench "readBit-1000" $ whnf (readBitN 1000) reader
  , bench "readBits-8-100" $ whnf (readBitsN 8 100) reader
  , bench "readBits-14-100" $ whnf (readBitsN 14 100) reader
  , bench "readBits-32-50" $ whnf (readBitsN 32 50) reader
  ]
  where
    readBitN :: Int -> BitReader -> Word32
    readBitN 0 _ = 0
    readBitN n r = let (b, r') = readBit r in if b then 1 + readBitN (n-1) r' else readBitN (n-1) r'

    readBitsN :: Int -> Int -> BitReader -> Word32
    readBitsN _ 0 _ = 0
    readBitsN bits n r = let (v, r') = readBits bits r in v + readBitsN bits (n-1) r'

-- | BitWriter benchmarks
bitWriterBenchmarks :: [Benchmark]
bitWriterBenchmarks =
  [ bench "empty" $ whnf (\_ -> emptyBitWriter) ()
  , bench "writeBits-8-1000" $ nf (writeBitsN 8 1000) ()
  , bench "writeBits-14-1000" $ nf (writeBitsN 14 1000) ()
  , bench "writeBit-10000" $ nf writeBitN 10000
  ]
  where
    finalize :: BitWriter -> B.ByteString
    finalize = bitWriterToByteString . finalizeBitWriter

    writeBitsN :: Int -> Int -> () -> B.ByteString
    writeBitsN numBits n _ = finalize $ go n emptyBitWriter
      where
        go 0 w = w
        go i w = go (i-1) (writeBits numBits 0xAB w)

    writeBitN :: Int -> B.ByteString
    writeBitN n = finalize $ go n emptyBitWriter
      where
        go 0 w = w
        go i w = go (i-1) (writeBit True w)

-- | DCT benchmarks
dctBenchmarks :: [Benchmark]
dctBenchmarks =
  [ bench "fdct4x4" $ nfIO $ runFDCT4x4 testBlock
  , bench "fdct4x4-batch-16" $ nfIO $ replicateM 16 (runFDCT4x4 testBlock)
  , bench "fdct4x4-batch-256" $ nfIO $ replicateM 256 (runFDCT4x4 testBlock)
  , bench "fwht4x4" $ nfIO $ runFWHT4x4 testBlock
  ]
  where
    testBlock :: VS.Vector Int16
    testBlock = VS.fromList [100, 110, 120, 130,
                             105, 115, 125, 135,
                             110, 120, 130, 140,
                             115, 125, 135, 145]

    runFDCT4x4 :: VS.Vector Int16 -> IO (VS.Vector Int16)
    runFDCT4x4 input = do
      result <- VS.thaw input
      stToIO $ fdct4x4 result
      VS.freeze result

    runFWHT4x4 :: VS.Vector Int16 -> IO (VS.Vector Int16)
    runFWHT4x4 input = do
      result <- VS.thaw input
      stToIO $ fwht4x4 result
      VS.freeze result

-- | IDCT benchmarks
idctBenchmarks :: [Benchmark]
idctBenchmarks =
  [ bench "idct4x4" $ nfIO $ runIDCT4x4 testCoeffs
  , bench "idct4x4-batch-16" $ nfIO $ replicateM 16 (runIDCT4x4 testCoeffs)
  , bench "idct4x4-batch-256" $ nfIO $ replicateM 256 (runIDCT4x4 testCoeffs)
  , bench "iwht4x4" $ nfIO $ runIWHT4x4 testCoeffs
  ]
  where
    testCoeffs :: VS.Vector Int16
    testCoeffs = VS.fromList [1000, 50, 30, 10,
                              40, 20, 15, 5,
                              25, 10, 8, 3,
                              12, 6, 4, 2]

    runIDCT4x4 :: VS.Vector Int16 -> IO (VS.Vector Int16)
    runIDCT4x4 input = do
      result <- VS.thaw input
      stToIO $ idct4x4 result
      VS.freeze result

    runIWHT4x4 :: VS.Vector Int16 -> IO (VS.Vector Int16)
    runIWHT4x4 input = do
      result <- VS.thaw input
      stToIO $ iwht4x4 result
      VS.freeze result

-- | Color conversion benchmarks (YCbCr <-> RGB simulation)
colorConvertBenchmarks :: [Benchmark]
colorConvertBenchmarks =
  [ bench "ycbcr-to-rgb-single" $ nf ycbcrToRGBSingle (128, 64, 192)
  , bench "ycbcr-to-rgb-row-16" $ nfIO $ runColorConvertRow 16
  , bench "ycbcr-to-rgb-row-256" $ nfIO $ runColorConvertRow 256
  , bench "ycbcr-to-rgb-block-16x16" $ nfIO $ runColorConvertBlock 16 16
  , bench "ycbcr-to-rgb-block-256x256" $ nfIO $ runColorConvertBlock 256 256
  ]
  where
    -- BT.601 YCbCr to RGB conversion (matching VP8 spec)
    ycbcrToRGBSingle :: (Word8, Word8, Word8) -> (Word8, Word8, Word8)
    ycbcrToRGBSingle (y, cb, cr) =
      let y' = fromIntegral y :: Int
          cb' = fromIntegral cb - 128 :: Int
          cr' = fromIntegral cr - 128 :: Int
          r = clip255 (y' + ((91881 * cr') `shiftR` 16))
          g = clip255 (y' - ((22554 * cb' + 46802 * cr') `shiftR` 16))
          b = clip255 (y' + ((116130 * cb') `shiftR` 16))
      in (r, g, b)

    clip255 :: Int -> Word8
    clip255 x
      | x < 0 = 0
      | x > 255 = 255
      | otherwise = fromIntegral x

    runColorConvertRow :: Int -> IO (VS.Vector Word8)
    runColorConvertRow width = do
      let yData = VS.replicate width (128 :: Word8)
          cbData = VS.replicate (width `div` 2) (64 :: Word8)
          crData = VS.replicate (width `div` 2) (192 :: Word8)
      rgbBuf <- VSM.new (width * 3)
      stToIO $ forM_ [0..width-1] $ \x -> do
        let y = yData VS.! x
            cb = cbData VS.! (x `div` 2)
            cr = crData VS.! (x `div` 2)
            (r, g, b) = ycbcrToRGBSingle (y, cb, cr)
        VSM.write rgbBuf (x * 3) r
        VSM.write rgbBuf (x * 3 + 1) g
        VSM.write rgbBuf (x * 3 + 2) b
      VS.freeze rgbBuf

    runColorConvertBlock :: Int -> Int -> IO (VS.Vector Word8)
    runColorConvertBlock width height = do
      let yData = VS.replicate (width * height) (128 :: Word8)
          cbData = VS.replicate ((width `div` 2) * (height `div` 2)) (64 :: Word8)
          crData = VS.replicate ((width `div` 2) * (height `div` 2)) (192 :: Word8)
      rgbBuf <- VSM.new (width * height * 3)
      stToIO $ forM_ [0..height-1] $ \row ->
        forM_ [0..width-1] $ \col -> do
          let idx = row * width + col
              chromaIdx = (row `div` 2) * (width `div` 2) + (col `div` 2)
              y = yData VS.! idx
              cb = cbData VS.! chromaIdx
              cr = crData VS.! chromaIdx
              (r, g, b) = ycbcrToRGBSingle (y, cb, cr)
          VSM.write rgbBuf (idx * 3) r
          VSM.write rgbBuf (idx * 3 + 1) g
          VSM.write rgbBuf (idx * 3 + 2) b
      VS.freeze rgbBuf

-- | Loop filter benchmarks (simulated filter operations)
loopFilterBenchmarks :: [Benchmark]
loopFilterBenchmarks =
  [ bench "normal-filter-edge" $ nfIO runNormalFilterEdge
  , bench "simple-filter-edge" $ nfIO runSimpleFilterEdge
  , bench "filter-macroblock-row" $ nfIO $ runFilterMacroblockRow 16
  ]
  where
    runNormalFilterEdge :: IO ()
    runNormalFilterEdge = do
      buf <- VSM.replicate 256 (128 :: Word8)
      stToIO $ do
        -- Simulate filtering a vertical edge
        forM_ [0..15] $ \row -> do
          let baseIdx = row * 16 + 8
          p0 <- VSM.read buf (baseIdx - 1)
          p1 <- VSM.read buf (baseIdx - 2)
          q0 <- VSM.read buf baseIdx
          q1 <- VSM.read buf (baseIdx + 1)
          -- Simple filter simulation
          let delta = clamp3 (fromIntegral q0 - fromIntegral p0)
          VSM.write buf (baseIdx - 1) (saturate $ fromIntegral p0 + delta)
          VSM.write buf baseIdx (saturate $ fromIntegral q0 - delta)
      return ()

    runSimpleFilterEdge :: IO ()
    runSimpleFilterEdge = do
      buf <- VSM.replicate 256 (128 :: Word8)
      stToIO $ do
        forM_ [0..15] $ \row -> do
          let baseIdx = row * 16 + 8
          p0 <- VSM.read buf (baseIdx - 1)
          q0 <- VSM.read buf baseIdx
          let delta = clamp3 (fromIntegral q0 - fromIntegral p0)
          VSM.write buf (baseIdx - 1) (saturate $ fromIntegral p0 + delta)
          VSM.write buf baseIdx (saturate $ fromIntegral q0 - delta)
      return ()

    runFilterMacroblockRow :: Int -> IO ()
    runFilterMacroblockRow numMBs = do
      buf <- VSM.replicate (numMBs * 16 * 16) (128 :: Word8)
      stToIO $ forM_ [0..numMBs-1] $ \mb -> do
        -- Filter vertical edges within macroblock
        forM_ [4, 8, 12] $ \col ->
          forM_ [0..15] $ \row -> do
            let baseIdx = mb * 256 + row * 16 + col
            when (baseIdx > 0 && baseIdx < numMBs * 256 - 1) $ do
              p0 <- VSM.read buf (baseIdx - 1)
              q0 <- VSM.read buf baseIdx
              let delta = clamp3 (fromIntegral q0 - fromIntegral p0)
              VSM.write buf (baseIdx - 1) (saturate $ fromIntegral p0 + delta)
              VSM.write buf baseIdx (saturate $ fromIntegral q0 - delta)
      return ()

    clamp3 :: Int -> Int
    clamp3 x = max (-3) (min 3 x)

    saturate :: Int -> Word8
    saturate x = fromIntegral $ max 0 (min 255 x)

-- | Boolean decoder benchmarks
boolDecoderBenchmarks :: [Benchmark]
boolDecoderBenchmarks =
  let testData = B.pack $ take 1024 $ cycle [0xAB, 0xCD, 0xEF, 0x12]
  in
  [ bench "init" $ whnf initBoolDecoder testData
  , bench "boolRead-prob128-1000" $ whnf (readBoolN 128 1000) (initBoolDecoder testData)
  , bench "boolRead-prob64-1000" $ whnf (readBoolN 64 1000) (initBoolDecoder testData)
  , bench "boolRead-prob192-1000" $ whnf (readBoolN 192 1000) (initBoolDecoder testData)
  ]
  where
    readBoolN :: Word8 -> Int -> BoolDecoder -> Int
    readBoolN _ 0 _ = 0
    readBoolN prob n dec =
      let (b, dec') = boolRead prob dec
      in (if b then 1 else 0) + readBoolN prob (n-1) dec'

-- | Boolean encoder benchmarks
boolEncoderBenchmarks :: [Benchmark]
boolEncoderBenchmarks =
  [ bench "init" $ whnf (\_ -> initBoolEncoder) ()
  , bench "boolWrite-prob128-1000" $ nf (writeBoolN 128 1000) ()
  , bench "boolWrite-prob64-1000" $ nf (writeBoolN 64 1000) ()
  ]
  where
    writeBoolN :: Word8 -> Int -> () -> B.ByteString
    writeBoolN prob n _ = finalizeBoolEncoder $ go n initBoolEncoder
      where
        go 0 enc = enc
        go i enc = go (i-1) (boolWrite prob True enc)

-- | Transform benchmarks (VP8L inverse transforms simulation)
transformBenchmarks :: [Benchmark]
transformBenchmarks =
  [ bench "subtract-green-inverse-256x256" $ nf applySubtractGreenInverse (generatePixelVector 256 256)
  , bench "subtract-green-inverse-512x512" $ nf applySubtractGreenInverse (generatePixelVector 512 512)
  , bench "predictor-vertical-256x256" $ nf (applyPredictorInverse 256) (generatePixelVector 256 256)
  ]
  where
    generatePixelVector :: Int -> Int -> VS.Vector Word32
    generatePixelVector w h = VS.generate (w * h) $ \i ->
      let x = i `mod` w
          y = i `div` w
          r = fromIntegral $ (x * 3 + y * 5) `mod` 256
          g = fromIntegral $ (x * 7 + y * 11) `mod` 256
          b = fromIntegral $ (x * 13 + y * 17) `mod` 256
          a = 255 :: Word32
      in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

    -- Inverse subtract green: red += green, blue += green
    applySubtractGreenInverse :: VS.Vector Word32 -> VS.Vector Word32
    applySubtractGreenInverse pixels = runST $ do
      mut <- VS.thaw pixels
      forM_ [0 .. VS.length pixels - 1] $ \i -> do
        argb <- VSM.read mut i
        let a = (argb `shiftR` 24) .&. 0xFF
            r = (argb `shiftR` 16) .&. 0xFF
            g = (argb `shiftR` 8) .&. 0xFF
            b = argb .&. 0xFF
            r' = (r + g) .&. 0xFF
            b' = (b + g) .&. 0xFF
        VSM.write mut i ((a `shiftL` 24) .|. (r' `shiftL` 16) .|. (g `shiftL` 8) .|. b')
      VS.freeze mut

    -- Simplified predictor inverse (vertical predictor mode 2)
    applyPredictorInverse :: Int -> VS.Vector Word32 -> VS.Vector Word32
    applyPredictorInverse width pixels = runST $ do
      mut <- VS.thaw pixels
      let height = VS.length pixels `div` width
      -- Process rows (skip first row)
      forM_ [1 .. height - 1] $ \y ->
        forM_ [0 .. width - 1] $ \x -> do
          let idx = y * width + x
              topIdx = (y - 1) * width + x
          current <- VSM.read mut idx
          top <- VSM.read mut topIdx
          -- Add top pixel to current (component-wise, mod 256)
          let a = ((current `shiftR` 24) + (top `shiftR` 24)) .&. 0xFF
              r = (((current `shiftR` 16) .&. 0xFF) + ((top `shiftR` 16) .&. 0xFF)) .&. 0xFF
              g = (((current `shiftR` 8) .&. 0xFF) + ((top `shiftR` 8) .&. 0xFF)) .&. 0xFF
              b = ((current .&. 0xFF) + (top .&. 0xFF)) .&. 0xFF
          VSM.write mut idx ((a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b)
      VS.freeze mut

-- | Roundtrip benchmarks (encode then decode)
roundtripBenchmarks :: [Benchmark]
roundtripBenchmarks =
  [ bgroup "lossless" $
      [ bench "64x64" $ nf roundtripLossless (generateTestImage 64 64)
      , bench "128x128" $ nf roundtripLossless (generateTestImage 128 128)
      , bench "256x256" $ nf roundtripLossless (generateTestImage 256 256)
      ]
  , bgroup "lossy-q50" $
      [ bench "64x64" $ nf (roundtripLossy 50) (generateTestImageRGB 64 64)
      , bench "128x128" $ nf (roundtripLossy 50) (generateTestImageRGB 128 128)
      , bench "256x256" $ nf (roundtripLossy 50) (generateTestImageRGB 256 256)
      ]
  ]
  where
    roundtripLossless :: Image PixelRGBA8 -> Either String DynamicImage
    roundtripLossless img = decodeWebP (encodeWebPLossless img)

    roundtripLossy :: Int -> Image PixelRGB8 -> Either String DynamicImage
    roundtripLossy q img = decodeWebP (encodeWebPLossy img q)

-- | Generate test RGBA image
generateTestImage :: Int -> Int -> Image PixelRGBA8
generateTestImage w h = generateImage genPixel w h
  where
    genPixel x y =
      let r = fromIntegral $ (x * 3 + y * 5) `mod` 256
          g = fromIntegral $ (x * 7 + y * 11) `mod` 256
          b = fromIntegral $ (x * 13 + y * 17) `mod` 256
      in PixelRGBA8 r g b 255

-- | Generate test RGB image
generateTestImageRGB :: Int -> Int -> Image PixelRGB8
generateTestImageRGB w h = generateImage genPixel w h
  where
    genPixel x y =
      let r = fromIntegral $ (x * 3 + y * 5) `mod` 256
          g = fromIntegral $ (x * 7 + y * 11) `mod` 256
          b = fromIntegral $ (x * 13 + y * 17) `mod` 256
      in PixelRGB8 r g b

-- | Generate solid color image
generateSolidImage :: Int -> Int -> PixelRGBA8 -> Image PixelRGBA8
generateSolidImage w h pixel = generateImage (\_ _ -> pixel) w h

-- | Generate gradient image
generateGradientImage :: Int -> Int -> Image PixelRGBA8
generateGradientImage w h = generateImage genPixel w h
  where
    genPixel x y =
      let r = fromIntegral $ (x * 255) `div` (max 1 (w - 1))
          g = fromIntegral $ (y * 255) `div` (max 1 (h - 1))
          b = fromIntegral $ ((x + y) * 127) `div` (max 1 (w + h - 2))
      in PixelRGBA8 r g b 255

-- | Generate checkerboard pattern
generateCheckerboard :: Int -> Int -> Image PixelRGBA8
generateCheckerboard w h = generateImage genPixel w h
  where
    genPixel x y =
      let isWhite = ((x `div` 16) + (y `div` 16)) `mod` 2 == 0
      in if isWhite then PixelRGBA8 255 255 255 255 else PixelRGBA8 0 0 0 255

-- | Generate noise-like pattern (deterministic)
generateNoiseImage :: Int -> Int -> Image PixelRGBA8
generateNoiseImage w h = generateImage genPixel w h
  where
    genPixel x y =
      let seed = x * 1103515245 + y * 12345 + x * y * 7919
          r = fromIntegral $ (seed `mod` 256)
          g = fromIntegral $ ((seed `div` 256) `mod` 256)
          b = fromIntegral $ ((seed `div` 65536) `mod` 256)
      in PixelRGBA8 r g b 255
