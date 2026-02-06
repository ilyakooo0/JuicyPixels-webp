{-# LANGUAGE OverloadedStrings #-}

-- | Simple example of using JuicyPixels-webp
module Main where

import Codec.Picture.Metadata (Metadatas, keys)
import Codec.Picture.Types
import Codec.Picture.WebP
import qualified Control.Exception
import qualified Data.ByteString as B
import System.IO.Error

main :: IO ()
main = do
  putStrLn "JuicyPixels-webp Simple Example\n"

  -- Example 1: Decode a WebP image
  putStrLn "Example 1: Basic decoding"
  result1 <- exampleDecode "test/data/test.webp"
  case result1 of
    Right dynImg -> putStrLn $ "✓ Decoded successfully: " ++ describeImage dynImg
    Left err -> putStrLn $ "✗ Decode failed: " ++ err

  putStrLn ""

  -- Example 2: Decode with metadata
  putStrLn "Example 2: Decoding with metadata"
  result2 <- exampleDecodeWithMetadata "test/data/test.webp"
  case result2 of
    Right (dynImg, _meta) -> do
      putStrLn $ "✓ Decoded with metadata"
      putStrLn $ "  Image: " ++ describeImage dynImg
    Left err -> putStrLn $ "✗ Decode failed: " ++ err

  putStrLn ""

  -- Example 3: Handle errors gracefully
  putStrLn "Example 3: Error handling"
  result3 <- exampleDecode "nonexistent.webp"
  case result3 of
    Right _ -> putStrLn "✗ Unexpected success"
    Left err -> putStrLn $ "✓ Properly handled error: " ++ take 50 err ++ "..."

  putStrLn "\nExamples complete!"

exampleDecode :: FilePath -> IO (Either String DynamicImage)
exampleDecode path = do
  fileExists <- doesFileExist path
  if not fileExists
    then return $ Left "File not found"
    else do
      webpData <- B.readFile path
      return $ decodeWebP webpData

exampleDecodeWithMetadata :: FilePath -> IO (Either String (DynamicImage, Metadatas))
exampleDecodeWithMetadata path = do
  fileExists <- doesFileExist path
  if not fileExists
    then return $ Left "File not found"
    else do
      webpData <- B.readFile path
      return $ decodeWebPWithMetadata webpData

describeImage :: DynamicImage -> String
describeImage dynImg = case dynImg of
  ImageRGBA8 img -> show (imageWidth img) ++ "x" ++ show (imageHeight img) ++ " RGBA8"
  ImageRGB8 img -> show (imageWidth img) ++ "x" ++ show (imageHeight img) ++ " RGB8"
  ImageY8 img -> show (imageWidth img) ++ "x" ++ show (imageHeight img) ++ " Y8"
  ImageYA8 img -> show (imageWidth img) ++ "x" ++ show (imageHeight img) ++ " YA8"
  _ -> "Unknown format"

doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
  result <- try (B.readFile path) :: IO (Either IOError B.ByteString)
  return $ case result of
    Right _ -> True
    Left _ -> False

try :: IO a -> IO (Either IOError a)
try action = Control.Exception.catch (Right <$> action) (return . Left)
