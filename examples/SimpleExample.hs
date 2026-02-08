{-# LANGUAGE OverloadedStrings #-}

-- | Simple example of using JuicyPixels-webp
module Main where

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
    Right dynImg -> putStrLn $ "Decoded successfully: " ++ describeImage dynImg
    Left err -> putStrLn $ "Decode failed: " ++ err

  putStrLn ""

  -- Example 2: Handle errors gracefully
  putStrLn "Example 2: Error handling"
  result2 <- exampleDecode "nonexistent.webp"
  case result2 of
    Right _ -> putStrLn "Unexpected success"
    Left err -> putStrLn $ "Properly handled error: " ++ take 50 err ++ "..."

  putStrLn "\nExamples complete!"

exampleDecode :: FilePath -> IO (Either String DynamicImage)
exampleDecode path = do
  fileExists <- doesFileExist path
  if not fileExists
    then return $ Left "File not found"
    else do
      webpData <- B.readFile path
      return $ decodeWebP webpData

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
