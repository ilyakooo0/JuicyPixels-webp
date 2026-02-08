#!/usr/bin/env stack
-- stack script --resolver lts-22.39 --package http-conduit --package bytestring

{-# LANGUAGE OverloadedStrings #-}

-- Script to download official WebP test images from Google's repository

import qualified Data.ByteString as B
import Network.HTTP.Simple
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  putStrLn "Downloading WebP test images from Google's repository..."
  createDirectoryIfMissing True "test/data/golden"

  -- Download various test images
  let images =
        [ ("https://www.gstatic.com/webp/gallery/1.webp", "test/data/golden/gallery1.webp", "VP8 photo")
        , ("https://www.gstatic.com/webp/gallery/2.webp", "test/data/golden/gallery2.webp", "VP8 photo")
        , ("https://www.gstatic.com/webp/gallery/3.webp", "test/data/golden/gallery3.webp", "VP8 photo")
        , ("https://www.gstatic.com/webp/gallery/4.webp", "test/data/golden/gallery4.webp", "VP8L graphics")
        , ("https://www.gstatic.com/webp/gallery/5.webp", "test/data/golden/gallery5.webp", "VP8L graphics")
        ]

  mapM_ (\(url, path, desc) -> do
    putStrLn $ "Downloading " ++ desc ++ " from " ++ url
    request <- parseRequest url
    response <- httpBS request
    let body = getResponseBody response
    B.writeFile path body
    putStrLn $ "  Saved to " ++ path ++ " (" ++ show (B.length body) ++ " bytes)"
    ) images

  putStrLn "\nDone! Test images downloaded to test/data/golden/"
  putStrLn "Run 'stack test' to verify decoding works with these images"
