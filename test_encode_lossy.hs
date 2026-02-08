#!/usr/bin/env stack
-- stack script --resolver lts-22.39 --package JuicyPixels --package bytestring

{-# LANGUAGE OverloadedStrings #-}

import Codec.Picture
import Codec.Picture.WebP
import qualified Data.ByteString as B

main :: IO ()
main = do
  putStrLn "Testing VP8 lossy encoding (interim implementation)..."

  -- Create a simple test image (16x16 red square)
  let img = generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16

  -- Encode at different quality levels
  let qualities = [10, 50, 80, 100]

  mapM_
    ( \q -> do
        let encoded = encodeWebPLossy img q
        putStrLn $ "Quality " ++ show q ++ ": " ++ show (B.length encoded) ++ " bytes"

        -- Try to decode it back
        case decodeWebP encoded of
          Right dynImg -> putStrLn $ "  ✓ Decodes successfully to " ++ describeDynImg dynImg
          Left err -> putStrLn $ "  ✗ Decode failed: " ++ err
    )
    qualities

  putStrLn "\nNote: Currently using VP8L lossless interim implementation"
  putStrLn "File sizes will be similar across quality levels until full VP8 encoder is complete"

describeDynImg :: DynamicImage -> String
describeDynImg (ImageRGB8 img) = show (imageWidth img) ++ "x" ++ show (imageHeight img) ++ " RGB8"
describeDynImg (ImageRGBA8 img) = show (imageWidth img) ++ "x" ++ show (imageHeight img) ++ " RGBA8"
describeDynImg _ = "other format"
