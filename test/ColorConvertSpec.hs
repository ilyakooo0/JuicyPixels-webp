{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ColorConvertSpec (spec) where

import Codec.Picture
import Codec.Picture.WebP.Internal.VP8.ColorConvert
import Control.Monad.ST
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "ColorConvert" $ do
  describe "clip255" $ do
    it "returns 0 for negative values" $ do
      clip255 (-1) `shouldBe` 0
      clip255 (-100) `shouldBe` 0
      clip255 (-256) `shouldBe` 0
      clip255 minBound `shouldBe` 0

    it "returns 255 for values > 255" $ do
      clip255 256 `shouldBe` 255
      clip255 300 `shouldBe` 255
      clip255 1000 `shouldBe` 255
      clip255 maxBound `shouldBe` 255

    it "returns value unchanged for valid range" $ do
      clip255 0 `shouldBe` 0
      clip255 128 `shouldBe` 128
      clip255 255 `shouldBe` 255

    it "handles all boundary values" $
      property $ \(x :: Int) ->
        let result = clip255 x
         in result >= 0 && result <= 255

  describe "rgbToYCbCr" $ do
    it "converts solid black image" $ do
      let img = generateImage (\_ _ -> PixelRGB8 0 0 0) 16 16
          (yBuf, uBuf, vBuf) = runST $ do
            (y, u, v) <- rgbToYCbCr img
            yFrozen <- VS.unsafeFreeze y
            uFrozen <- VS.unsafeFreeze u
            vFrozen <- VS.unsafeFreeze v
            return (yFrozen, uFrozen, vFrozen)

      -- Black should produce Y=0 (or close to it), Cb=Cr=128 (neutral)
      VS.head yBuf `shouldSatisfy` (< 5)
      VS.head uBuf `shouldSatisfy` (\x -> x >= 125 && x <= 131)
      VS.head vBuf `shouldSatisfy` (\x -> x >= 125 && x <= 131)

    it "converts solid white image" $ do
      let img = generateImage (\_ _ -> PixelRGB8 255 255 255) 16 16
          (yBuf, uBuf, vBuf) = runST $ do
            (y, u, v) <- rgbToYCbCr img
            yFrozen <- VS.unsafeFreeze y
            uFrozen <- VS.unsafeFreeze u
            vFrozen <- VS.unsafeFreeze v
            return (yFrozen, uFrozen, vFrozen)

      -- White should produce Y=255, Cb=Cr=128 (neutral)
      VS.head yBuf `shouldSatisfy` (> 250)
      VS.head uBuf `shouldSatisfy` (\x -> x >= 125 && x <= 131)
      VS.head vBuf `shouldSatisfy` (\x -> x >= 125 && x <= 131)

    it "converts solid red image" $ do
      let img = generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16
          (yBuf, uBuf, vBuf) = runST $ do
            (y, u, v) <- rgbToYCbCr img
            yFrozen <- VS.unsafeFreeze y
            uFrozen <- VS.unsafeFreeze u
            vFrozen <- VS.unsafeFreeze v
            return (yFrozen, uFrozen, vFrozen)

      -- Red should produce Y~76, Cb<128, Cr>128
      VS.head yBuf `shouldSatisfy` (\y -> y >= 70 && y <= 85)
      VS.head uBuf `shouldSatisfy` (< 128) -- Blue-yellow axis
      VS.head vBuf `shouldSatisfy` (> 128) -- Red-cyan axis

    it "converts solid green image" $ do
      let img = generateImage (\_ _ -> PixelRGB8 0 255 0) 16 16
          (yBuf, uBuf, vBuf) = runST $ do
            (y, u, v) <- rgbToYCbCr img
            yFrozen <- VS.unsafeFreeze y
            uFrozen <- VS.unsafeFreeze u
            vFrozen <- VS.unsafeFreeze v
            return (yFrozen, uFrozen, vFrozen)

      -- Green should produce high Y (main luma contributor), Cb<128, Cr<128
      VS.head yBuf `shouldSatisfy` (\y -> y >= 145 && y <= 155)
      VS.head uBuf `shouldSatisfy` (< 128)
      VS.head vBuf `shouldSatisfy` (< 128)

    it "converts solid blue image" $ do
      let img = generateImage (\_ _ -> PixelRGB8 0 0 255) 16 16
          (yBuf, uBuf, vBuf) = runST $ do
            (y, u, v) <- rgbToYCbCr img
            yFrozen <- VS.unsafeFreeze y
            uFrozen <- VS.unsafeFreeze u
            vFrozen <- VS.unsafeFreeze v
            return (yFrozen, uFrozen, vFrozen)

      -- Blue should produce low Y, Cb>128, Cr<128
      VS.head yBuf `shouldSatisfy` (\y -> y >= 25 && y <= 35)
      VS.head uBuf `shouldSatisfy` (> 128)
      VS.head vBuf `shouldSatisfy` (< 128)

    it "produces correct buffer sizes (padded to macroblock)" $ do
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 33 17
          (yBuf, uBuf, vBuf) = runST $ do
            (y, u, v) <- rgbToYCbCr img
            yFrozen <- VS.unsafeFreeze y
            uFrozen <- VS.unsafeFreeze u
            vFrozen <- VS.unsafeFreeze v
            return (yFrozen, uFrozen, vFrozen)

      -- 33x17 -> padded to 48x32
      VS.length yBuf `shouldBe` (48 * 32)
      -- Chroma is half size: 24x16
      VS.length uBuf `shouldBe` (24 * 16)
      VS.length vBuf `shouldBe` (24 * 16)

    it "pads small images correctly" $ do
      let img = generateImage (\_ _ -> PixelRGB8 100 100 100) 1 1
          (yBuf, uBuf, vBuf) = runST $ do
            (y, u, v) <- rgbToYCbCr img
            yFrozen <- VS.unsafeFreeze y
            uFrozen <- VS.unsafeFreeze u
            vFrozen <- VS.unsafeFreeze v
            return (yFrozen, uFrozen, vFrozen)

      -- 1x1 -> padded to 16x16
      VS.length yBuf `shouldBe` (16 * 16)
      VS.length uBuf `shouldBe` (8 * 8)
      VS.length vBuf `shouldBe` (8 * 8)

    it "handles gradient image" $ do
      let img = generateImage (\x _ -> PixelRGB8 (fromIntegral x * 16) 128 128) 16 16
          (yBuf, _, _) = runST $ do
            (y, u, v) <- rgbToYCbCr img
            yFrozen <- VS.unsafeFreeze y
            uFrozen <- VS.unsafeFreeze u
            vFrozen <- VS.unsafeFreeze v
            return (yFrozen, uFrozen, vFrozen)

      -- First column should have lower luma than last column
      let y0 = yBuf VS.! 0
          y15 = yBuf VS.! 15
      y15 `shouldSatisfy` (> y0)

  describe "Color Space Properties" $ do
    it "grayscale produces neutral chroma" $
      property $ \(gray :: Word8) ->
        let img = generateImage (\_ _ -> PixelRGB8 gray gray gray) 16 16
            (_, uBuf, vBuf) = runST $ do
              (y, u, v) <- rgbToYCbCr img
              yFrozen <- VS.unsafeFreeze y
              uFrozen <- VS.unsafeFreeze u
              vFrozen <- VS.unsafeFreeze v
              return (yFrozen, uFrozen, vFrozen)
            u = VS.head uBuf
            v = VS.head vBuf
         in abs (fromIntegral u - 128 :: Int) < 5
              && abs (fromIntegral v - 128 :: Int) < 5

    it "luma increases with brightness" $
      property $ \(r :: Word8, g :: Word8, b :: Word8) ->
        let img1 = generateImage (\_ _ -> PixelRGB8 r g b) 16 16
            img2 = generateImage (\_ _ -> PixelRGB8 (r `div` 2) (g `div` 2) (b `div` 2)) 16 16
            getY img = runST $ do
              (y, _, _) <- rgbToYCbCr img
              yFrozen <- VS.unsafeFreeze y
              return (VS.head yFrozen)
            y1 = getY img1
            y2 = getY img2
         in y1 >= y2 -- Brighter image should have >= luma

    it "output values are always in valid range [0,255]" $
      property $ \(r :: Word8, g :: Word8, b :: Word8) ->
        let img = generateImage (\_ _ -> PixelRGB8 r g b) 16 16
            (yBuf, uBuf, vBuf) = runST $ do
              (y, u, v) <- rgbToYCbCr img
              yFrozen <- VS.unsafeFreeze y
              uFrozen <- VS.unsafeFreeze u
              vFrozen <- VS.unsafeFreeze v
              return (yFrozen, uFrozen, vFrozen)
         in VS.all (<= 255) yBuf
              && VS.all (<= 255) uBuf
              && VS.all (<= 255) vBuf

  describe "Chroma Subsampling" $ do
    it "chroma is half the luma resolution" $ do
      let img = generateImage (\_ _ -> PixelRGB8 128 128 128) 32 32
          (yBuf, uBuf, vBuf) = runST $ do
            (y, u, v) <- rgbToYCbCr img
            yFrozen <- VS.unsafeFreeze y
            uFrozen <- VS.unsafeFreeze u
            vFrozen <- VS.unsafeFreeze v
            return (yFrozen, uFrozen, vFrozen)

      VS.length yBuf `shouldBe` (32 * 32)
      VS.length uBuf `shouldBe` (16 * 16)
      VS.length vBuf `shouldBe` (16 * 16)

    it "samples from even coordinates" $ do
      -- Create image with different colors at even/odd positions
      let img =
            generateImage
              ( \x y ->
                  if even x && even y
                    then PixelRGB8 255 0 0 -- Red at even positions
                    else PixelRGB8 0 0 255 -- Blue at odd positions
              )
              16
              16
          (_, uBuf, vBuf) = runST $ do
            (y, u, v) <- rgbToYCbCr img
            yFrozen <- VS.unsafeFreeze y
            uFrozen <- VS.unsafeFreeze u
            vFrozen <- VS.unsafeFreeze v
            return (yFrozen, uFrozen, vFrozen)

      -- Should sample the red pixels (even x, even y)
      -- Red has low Cb and high Cr
      VS.head uBuf `shouldSatisfy` (< 128)
      VS.head vBuf `shouldSatisfy` (> 128)
