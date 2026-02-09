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
-- Optimized with separate loops for edges and interior to eliminate boundary checks
applyAlphaFiltering :: Int -> Int -> Word8 -> VS.Vector Word8 -> Either String (VS.Vector Word8)
applyAlphaFiltering width height filterMethod alphaData
  | filterMethod == 0 = return alphaData
  | otherwise = Right $ runST $ do
      output <- VSM.new (width * height)

      case filterMethod of
        1 -> applyHorizontalFilter width height alphaData output
        2 -> applyVerticalFilter width height alphaData output
        3 -> applyGradientFilter width height alphaData output
        _ ->
          -- Just copy the data unchanged
          forM_ [0 .. width * height - 1] $ \i ->
            VSM.unsafeWrite output i (alphaData `VS.unsafeIndex` i)

      VS.unsafeFreeze output

-- | Horizontal filter: left prediction (filter method 1)
{-# INLINE applyHorizontalFilter #-}
applyHorizontalFilter :: Int -> Int -> VS.Vector Word8 -> VSM.MVector s Word8 -> ST s ()
applyHorizontalFilter width height alphaData output = do
  -- First column: no left neighbor (predict from 0)
  forM_ [0 .. height - 1] $ \y -> do
    let !idx = y * width
        !encoded = alphaData `VS.unsafeIndex` idx
        !decoded = fromIntegral encoded :: Word8
    VSM.unsafeWrite output idx decoded

  -- Rest of columns: has left neighbor (no boundary check needed)
  forM_ [0 .. height - 1] $ \y -> do
    let !rowBase = y * width
    forM_ [1 .. width - 1] $ \x -> do
      let !idx = rowBase + x
          !encoded = alphaData `VS.unsafeIndex` idx
      !left <- VSM.unsafeRead output (idx - 1)
      let !decoded = fromIntegral ((fromIntegral encoded + fromIntegral left) .&. 0xFF :: Int) :: Word8
      VSM.unsafeWrite output idx decoded

-- | Vertical filter: above prediction (filter method 2)
{-# INLINE applyVerticalFilter #-}
applyVerticalFilter :: Int -> Int -> VS.Vector Word8 -> VSM.MVector s Word8 -> ST s ()
applyVerticalFilter width height alphaData output = do
  -- First row: no above neighbor (predict from 0)
  forM_ [0 .. width - 1] $ \x -> do
    let !encoded = alphaData `VS.unsafeIndex` x
        !decoded = fromIntegral encoded :: Word8
    VSM.unsafeWrite output x decoded

  -- Rest of rows: has above neighbor (no boundary check needed)
  forM_ [1 .. height - 1] $ \y -> do
    let !rowBase = y * width
        !prevRowBase = (y - 1) * width
    forM_ [0 .. width - 1] $ \x -> do
      let !idx = rowBase + x
          !encoded = alphaData `VS.unsafeIndex` idx
      !above <- VSM.unsafeRead output (prevRowBase + x)
      let !decoded = fromIntegral ((fromIntegral encoded + fromIntegral above) .&. 0xFF :: Int) :: Word8
      VSM.unsafeWrite output idx decoded

-- | Gradient filter: left + above - above_left prediction (filter method 3)
{-# INLINE applyGradientFilter #-}
applyGradientFilter :: Int -> Int -> VS.Vector Word8 -> VSM.MVector s Word8 -> ST s ()
applyGradientFilter width height alphaData output = do
  -- Top-left corner: no neighbors (predict from 0)
  let !encoded00 = alphaData `VS.unsafeIndex` 0
  VSM.unsafeWrite output 0 (fromIntegral encoded00)

  -- First row (except first pixel): only left neighbor
  forM_ [1 .. width - 1] $ \x -> do
    let !encoded = alphaData `VS.unsafeIndex` x
    !left <- VSM.unsafeRead output (x - 1)
    let !decoded = fromIntegral ((fromIntegral encoded + fromIntegral left) .&. 0xFF :: Int) :: Word8
    VSM.unsafeWrite output x decoded

  -- First column (except first pixel): only above neighbor
  forM_ [1 .. height - 1] $ \y -> do
    let !idx = y * width
        !encoded = alphaData `VS.unsafeIndex` idx
    !above <- VSM.unsafeRead output ((y - 1) * width)
    let !decoded = fromIntegral ((fromIntegral encoded + fromIntegral above) .&. 0xFF :: Int) :: Word8
    VSM.unsafeWrite output idx decoded

  -- Interior pixels: all neighbors available (no boundary checks!)
  when (width > 1 && height > 1) $
    forM_ [1 .. height - 1] $ \y -> do
      let !rowBase = y * width
          !prevRowBase = (y - 1) * width
      forM_ [1 .. width - 1] $ \x -> do
        let !idx = rowBase + x
            !encoded = alphaData `VS.unsafeIndex` idx
        !left <- VSM.unsafeRead output (idx - 1)
        !above <- VSM.unsafeRead output (prevRowBase + x)
        !aboveLeft <- VSM.unsafeRead output (prevRowBase + x - 1)
        let !pred = clip255Int (fromIntegral left + fromIntegral above - fromIntegral aboveLeft)
            !decoded = fromIntegral ((fromIntegral encoded + pred) .&. 0xFF :: Int) :: Word8
        VSM.unsafeWrite output idx decoded

{-# INLINE clip255Int #-}
clip255Int :: Int -> Int
clip255Int !x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = x
