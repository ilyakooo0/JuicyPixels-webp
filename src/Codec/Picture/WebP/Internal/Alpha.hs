{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.Alpha
  ( decodeAlpha,
  )
where

import Codec.Picture.WebP.Internal.BitReader
import Codec.Picture.WebP.Internal.VP8L
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word

-- Performance: Fast paths for interior pixels eliminate boundary checks

-- | Decode ALPH chunk to produce alpha plane
decodeAlpha :: Int -> Int -> B.ByteString -> Either String (VS.Vector Word8)
decodeAlpha width height bs
  | B.null bs = Left "Empty ALPH chunk"
  | otherwise = do
      let headerByte = B.index bs 0
          _reserved = headerByte `shiftR` 6
          preprocessing = (headerByte `shiftR` 4) .&. 0x03
          filtering = (headerByte `shiftR` 2) .&. 0x03
          compression = headerByte .&. 0x03

      alphaData <-
        if compression == 0
          then Right $ VS.fromList $ B.unpack $ B.drop 1 bs
          else do
            let vp8lData = B.drop 1 bs
            pixels <- decodeVP8LHeaderless width height vp8lData

            let alphaVals = VS.generate (width * height) $ \i ->
                  let pixel = pixels VS.! i
                   in fromIntegral ((pixel `shiftR` 8) .&. 0xFF)

            return alphaVals

      filtered <- applyAlphaFiltering width height filtering alphaData
      return filtered

-- | Apply alpha filtering
applyAlphaFiltering :: Int -> Int -> Word8 -> VS.Vector Word8 -> Either String (VS.Vector Word8)
applyAlphaFiltering width height filterMethod alphaData
  | filterMethod == 0 = return alphaData
  | otherwise = Right $ runST $ do
      output <- VSM.new (width * height)

      forM_ [0 .. height - 1] $ \y ->
        forM_ [0 .. width - 1] $ \x -> do
          let idx = y * width + x
              encoded = alphaData VS.! idx

          decoded <- case filterMethod of
            1 -> do
              left <-
                if x > 0
                  then VSM.read output (y * width + x - 1)
                  else return 0
              return $ (fromIntegral encoded + fromIntegral left) `mod` 256
            2 -> do
              above <-
                if y > 0
                  then VSM.read output ((y - 1) * width + x)
                  else return 0
              return $ (fromIntegral encoded + fromIntegral above) `mod` 256
            3 -> do
              left <-
                if x > 0
                  then VSM.read output (y * width + x - 1)
                  else return 0
              above <-
                if y > 0
                  then VSM.read output ((y - 1) * width + x)
                  else return 0
              aboveLeft <-
                if x > 0 && y > 0
                  then VSM.read output ((y - 1) * width + x - 1)
                  else return 0

              let pred = clip255 (fromIntegral left + fromIntegral above - fromIntegral aboveLeft)
                  val = (fromIntegral encoded + pred) `mod` 256
              return val
            _ -> return $ fromIntegral encoded

          VSM.write output idx (fromIntegral decoded :: Word8)

      VS.unsafeFreeze output

clip255 :: Int -> Int
clip255 x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = x
