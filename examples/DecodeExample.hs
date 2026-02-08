{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Picture.Png (encodePng)
import Codec.Picture.Types
import Codec.Picture.WebP
import qualified Data.ByteString as B
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: decode-example <input.webp> [output.png]"
    [input] -> decodeAndShow input Nothing
    [input, output] -> decodeAndShow input (Just output)
    _ -> putStrLn "Too many arguments"

decodeAndShow :: FilePath -> Maybe FilePath -> IO ()
decodeAndShow inputPath maybeOutputPath = do
  putStrLn $ "Reading " ++ inputPath ++ "..."
  webpData <- B.readFile inputPath

  putStrLn "Decoding WebP image..."
  case decodeWebP webpData of
    Left err -> do
      putStrLn $ "Error: " ++ err
      exitFailure
    Right dynImg -> do
      putStrLn "Successfully decoded!"
      showImageInfo dynImg

      case maybeOutputPath of
        Nothing -> putStrLn "\nNo output path specified, skipping save."
        Just outPath -> do
          putStrLn $ "\nSaving to " ++ outPath ++ "..."
          case dynImg of
            ImageRGBA8 img -> do
              B.writeFile outPath (encodePng img)
              putStrLn "Saved as PNG"
            ImageRGB8 img -> do
              B.writeFile outPath (encodePng img)
              putStrLn "Saved as PNG"
            _ -> putStrLn "Unsupported image format for PNG export"

showImageInfo :: DynamicImage -> IO ()
showImageInfo dynImg = do
  case dynImg of
    ImageRGBA8 img -> do
      putStrLn $ "Format: RGBA8"
      putStrLn $ "Dimensions: " ++ show (imageWidth img) ++ "x" ++ show (imageHeight img)
    ImageRGB8 img -> do
      putStrLn $ "Format: RGB8"
      putStrLn $ "Dimensions: " ++ show (imageWidth img) ++ "x" ++ show (imageHeight img)
    _ -> putStrLn "Format: Unknown"
