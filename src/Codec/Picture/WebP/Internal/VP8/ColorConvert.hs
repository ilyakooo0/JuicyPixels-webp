{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Picture.WebP.Internal.VP8.ColorConvert
  ( rgbToYCbCr,
    rgbToYCbCrSharp,
    clip255,
  )
where

import Codec.Picture.Types
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import Data.Int (Int16)
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Clip value to [0, 255] range
{-# INLINE clip255 #-}
clip255 :: Int -> Word8
clip255 !x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = fromIntegral x

-- | Convert RGB8 image to YCbCr planes
-- Returns (Y buffer, U buffer, V buffer)
-- Y plane is full resolution (width × height)
-- U and V planes are subsampled (width/2 × height/2)
-- Uses BT.601 conversion with 4:2:0 chroma subsampling via 2×2 box filter
rgbToYCbCr ::
  Image PixelRGB8 ->
  ST s (VSM.MVector s Word8, VSM.MVector s Word8, VSM.MVector s Word8)
rgbToYCbCr img = do
  let w = imageWidth img
      h = imageHeight img
      !paddedW = ((w + 15) `shiftR` 4) `shiftL` 4
      !paddedH = ((h + 15) `shiftR` 4) `shiftL` 4
      !chromaW = paddedW `shiftR` 1
      !chromaH = paddedH `shiftR` 1

  yBuf <- VSM.replicate (paddedW * paddedH) 128
  uBuf <- VSM.replicate (chromaW * chromaH) 128
  vBuf <- VSM.replicate (chromaW * chromaH) 128

  -- Pass 1: Luma at full resolution
  forM_ [0 .. h - 1] $ \y ->
    forM_ [0 .. w - 1] $ \x -> do
      let PixelRGB8 r g b = pixelAt img x y
          -- BT.601 fixed-point (scaled by 256):
          -- Y = (77*R + 150*G + 29*B + 128) >> 8
          !r' = fromIntegral r :: Int
          !g' = fromIntegral g :: Int
          !b' = fromIntegral b :: Int
      VSM.write yBuf (y * paddedW + x) $!
        clip255 $ (77 * r' + 150 * g' + 29 * b' + 128) `shiftR` 8

  -- Pass 2: Chroma with 2×2 box filter subsampling
  -- Each chroma sample averages Cb/Cr over a 2×2 pixel block.
  -- At image edges, clamp to the nearest valid pixel.
  let !chromaRows = (h + 1) `shiftR` 1
      !chromaCols = (w + 1) `shiftR` 1
  forM_ [0 .. chromaRows - 1] $ \cy ->
    forM_ [0 .. chromaCols - 1] $ \cx -> do
      let !x0 = cx `shiftL` 1
          !y0 = cy `shiftL` 1
          !x1 = min (x0 + 1) (w - 1)
          !y1 = min (y0 + 1) (h - 1)
          -- Compute unbiased chroma per pixel (before +128 offset)
          -- Cb_raw = (-43*R - 85*G + 128*B + 128) >> 8
          -- Cr_raw = (128*R - 107*G - 21*B + 128) >> 8
          getCbCr !px !py =
            let PixelRGB8 r g b = pixelAt img px py
                !r' = fromIntegral r :: Int
                !g' = fromIntegral g :: Int
                !b' = fromIntegral b :: Int
                !cb = (-43 * r' - 85 * g' + 128 * b' + 128) `shiftR` 8
                !cr = (128 * r' - 107 * g' - 21 * b' + 128) `shiftR` 8
             in (cb, cr)
          !(cb00, cr00) = getCbCr x0 y0
          !(cb10, cr10) = getCbCr x1 y0
          !(cb01, cr01) = getCbCr x0 y1
          !(cb11, cr11) = getCbCr x1 y1
          -- Average with rounding bias (+2 for round-half-up on >>2), then add offset
          !avgCb = clip255 $ ((cb00 + cb10 + cb01 + cb11 + 2) `shiftR` 2) + 128
          !avgCr = clip255 $ ((cr00 + cr10 + cr01 + cr11 + 2) `shiftR` 2) + 128
      VSM.write uBuf (cy * chromaW + cx) avgCb
      VSM.write vBuf (cy * chromaW + cx) avgCr

  return (yBuf, uBuf, vBuf)

-- ---------------------------------------------------------------------------
-- Sharp YUV conversion (iterative chroma optimization)
-- ---------------------------------------------------------------------------
-- Based on libwebp's SharpYUV by Pascal Massimino.
-- Iteratively adjusts luma and chroma so that when the decoder upsamples
-- chroma with bilinear interpolation and converts back to RGB, the result
-- is as close as possible to the original. Uses sRGB gamma for perceptually
-- correct averaging.

-- | sRGB forward gamma LUT: 10-bit gamma (0-1023) → 16-bit linear (0-65535).
-- Input values are 8-bit RGB shifted left by 2 for extra precision.
gammaToLinearTab :: VU.Vector Word16
gammaToLinearTab = VU.generate 1024 $ \i ->
  let x = fromIntegral i / 1023.0 :: Double
      lin =
        if x <= 0.04045
          then x / 12.92
          else ((x + 0.055) / 1.055) ** 2.4
   in round (max 0 (min 65535 (lin * 65535.0)))

-- | sRGB inverse gamma LUT: 16-bit linear (0-65535) → 10-bit gamma (0-1023).
linearToGammaTab :: VU.Vector Word16
linearToGammaTab = VU.generate 65536 $ \i ->
  let x = fromIntegral i / 65535.0 :: Double
      gam =
        if x <= 0.0031308
          then 12.92 * x
          else 1.055 * x ** (1.0 / 2.4) - 0.055
   in round (max 0 (min 1023 (gam * 1023.0)))

{-# INLINE gammaToLinear #-}
gammaToLinear :: Int -> Int
gammaToLinear !g = fromIntegral (gammaToLinearTab `VU.unsafeIndex` max 0 (min 1023 g))

{-# INLINE linearToGamma #-}
linearToGamma :: Int -> Int
linearToGamma !lin = fromIntegral (linearToGammaTab `VU.unsafeIndex` max 0 (min 65535 lin))

-- | BT.601 weighted grayscale. Preserves input scale (works at any bit depth).
-- (77 + 150 + 29 = 256, so (77R + 150G + 29B + 128) >> 8 ≈ 0.30R + 0.59G + 0.11B)
{-# INLINE rgbToGray #-}
rgbToGray :: Int -> Int -> Int -> Int
rgbToGray !r !g !b = (77 * r + 150 * g + 29 * b + 128) `shiftR` 8

-- | Gamma-aware 2×2 downsample: linearize each pixel, average, convert back.
{-# INLINE scaleDown4 #-}
scaleDown4 :: Int -> Int -> Int -> Int -> Int
scaleDown4 !a !b !c !d =
  linearToGamma $
    (gammaToLinear a + gammaToLinear b + gammaToLinear c + gammaToLinear d + 2) `shiftR` 2

-- | Clip to 10-bit range [0, 1023]
{-# INLINE clip10 #-}
clip10 :: Int -> Int
clip10 !x
  | x < 0 = 0
  | x > 1023 = 1023
  | otherwise = x

-- | Convert RGB8 image to YCbCr planes using Sharp YUV.
-- Iteratively optimizes chroma values to minimize reconstruction error
-- after bilinear upsampling, using sRGB gamma-aware computations.
-- Produces significantly better color fidelity at sharp edges compared
-- to simple box-filter downsampling.
rgbToYCbCrSharp :: forall s.
  Image PixelRGB8 ->
  ST s (VSM.MVector s Word8, VSM.MVector s Word8, VSM.MVector s Word8)
rgbToYCbCrSharp img = do
  let w = imageWidth img
      h = imageHeight img
      !paddedW = ((w + 15) `shiftR` 4) `shiftL` 4
      !paddedH = ((h + 15) `shiftR` 4) `shiftL` 4
      !chromaW = paddedW `shiftR` 1
      !chromaH = paddedH `shiftR` 1
      -- Working dimensions rounded up to even (for 2×2 chroma blocks)
      !workW = (w + 1) .&. complement 1
      !workH = (h + 1) .&. complement 1
      !uvW = workW `shiftR` 1
      !uvH = workH `shiftR` 1
      !uvSize = uvW * uvH
      !pixSize = workW * workH

  -- Output buffers (same format as rgbToYCbCr)
  yBuf <- VSM.replicate (paddedW * paddedH) 128
  uBuf <- VSM.replicate (chromaW * chromaH) 128
  vBuf <- VSM.replicate (chromaW * chromaH) 128

  -- Working buffers at 10-bit precision (8-bit input << 2)
  bestY <- VSM.new pixSize :: ST s (VSM.MVector s Int16)
  targetY <- VSM.new pixSize :: ST s (VSM.MVector s Int16)
  bestUVr <- VSM.new uvSize :: ST s (VSM.MVector s Int16)
  bestUVg <- VSM.new uvSize :: ST s (VSM.MVector s Int16)
  bestUVb <- VSM.new uvSize :: ST s (VSM.MVector s Int16)
  targetUVr <- VSM.new uvSize :: ST s (VSM.MVector s Int16)
  targetUVg <- VSM.new uvSize :: ST s (VSM.MVector s Int16)
  targetUVb <- VSM.new uvSize :: ST s (VSM.MVector s Int16)
  -- Reconstruction temporaries (reused across iterations)
  reconYBuf <- VSM.new pixSize :: ST s (VSM.MVector s Int16)
  reconUVr <- VSM.new uvSize :: ST s (VSM.MVector s Int16)
  reconUVg <- VSM.new uvSize :: ST s (VSM.MVector s Int16)
  reconUVb <- VSM.new uvSize :: ST s (VSM.MVector s Int16)

  -- -----------------------------------------------------------------------
  -- Phase 1: Initialize bestY, targetY, bestUV, targetUV
  -- -----------------------------------------------------------------------

  -- Per-pixel: bestY = gamma-domain gray, targetY = gamma-aware gray
  forM_ [0 .. workH - 1] $ \py ->
    forM_ [0 .. workW - 1] $ \px -> do
      let !cx = min px (w - 1)
          !cy = min py (h - 1)
          PixelRGB8 r8 g8 b8 = pixelAt img cx cy
          !r10 = fromIntegral r8 `shiftL` 2 :: Int
          !g10 = fromIntegral g8 `shiftL` 2 :: Int
          !b10 = fromIntegral b8 `shiftL` 2 :: Int
          !gray = rgbToGray r10 g10 b10
          !tgt = linearToGamma (rgbToGray (gammaToLinear r10) (gammaToLinear g10) (gammaToLinear b10))
          !idx = py * workW + px
      VSM.unsafeWrite bestY idx (fromIntegral gray)
      VSM.unsafeWrite targetY idx (fromIntegral tgt)

  -- Per-UV-block: gamma-aware downsample → chroma residuals
  let getPixel10 !px !py =
        let !ax = min px (w - 1)
            !ay = min py (h - 1)
            PixelRGB8 r8 g8 b8 = pixelAt img ax ay
         in ( fromIntegral r8 `shiftL` 2 :: Int,
              fromIntegral g8 `shiftL` 2 :: Int,
              fromIntegral b8 `shiftL` 2 :: Int
            )
  forM_ [0 .. uvH - 1] $ \cy ->
    forM_ [0 .. uvW - 1] $ \cx -> do
      let !(ra, ga, ba) = getPixel10 (cx * 2) (cy * 2)
          !(rb, gb, bb) = getPixel10 (cx * 2 + 1) (cy * 2)
          !(rc, gc, bc) = getPixel10 (cx * 2) (cy * 2 + 1)
          !(rd, gd, bd) = getPixel10 (cx * 2 + 1) (cy * 2 + 1)
          !rDown = scaleDown4 ra rb rc rd
          !gDown = scaleDown4 ga gb gc gd
          !bDown = scaleDown4 ba bb bc bd
          !wg = rgbToGray rDown gDown bDown
          !idx = cy * uvW + cx
      VSM.unsafeWrite targetUVr idx $! fromIntegral (rDown - wg)
      VSM.unsafeWrite targetUVg idx $! fromIntegral (gDown - wg)
      VSM.unsafeWrite targetUVb idx $! fromIntegral (bDown - wg)
      VSM.unsafeWrite bestUVr idx $! fromIntegral (rDown - wg)
      VSM.unsafeWrite bestUVg idx $! fromIntegral (gDown - wg)
      VSM.unsafeWrite bestUVb idx $! fromIntegral (bDown - wg)

  -- -----------------------------------------------------------------------
  -- Phase 2: Iterative optimization (up to 4 iterations)
  -- -----------------------------------------------------------------------

  -- Helper: bilinear interpolation of one UV channel at full-res pixel (px, py).
  -- Uses the 9/3/3/1 kernel matching the VP8 "fancy" upsampler.
  let interpChan :: VSM.MVector s Int16 -> Int -> Int -> ST s Int
      interpChan !buf !px !py = do
        let !ucx = px `shiftR` 1
            !ucy = py `shiftR` 1
            !dx = px .&. 1
            !ucx1 = min (uvW - 1) (ucx + 1)
            -- Closer row = current UV row; farther = row above (dy=0) or below (dy=1)
            !ucy_far =
              if (py .&. 1) == 0
                then max 0 (ucy - 1)
                else min (uvH - 1) (ucy + 1)
            !ai = ucy * uvW + ucx
            !bi = ucy * uvW + ucx1
            !ci = ucy_far * uvW + ucx
            !di = ucy_far * uvW + ucx1
        !a <- fromIntegral <$> VSM.unsafeRead buf ai
        !b <- fromIntegral <$> VSM.unsafeRead buf bi
        !c <- fromIntegral <$> VSM.unsafeRead buf ci
        !d <- fromIntegral <$> VSM.unsafeRead buf di
        return $!
          if dx == 0
            then (9 * a + 3 * b + 3 * c + d + 8) `shiftR` 4
            else (3 * a + 9 * b + c + 3 * d + 8) `shiftR` 4

  let iterate_ :: Int -> Int -> ST s ()
      iterate_ !iter !prevDiffSum
        | iter >= 4 = return ()
        | otherwise = do
            -- Step A: Compute reconstructed UV for all blocks
            forM_ [0 .. uvH - 1] $ \cy ->
              forM_ [0 .. uvW - 1] $ \cx -> do
                let !px0 = cx * 2
                    !py0 = cy * 2
                    reconPix :: Int -> Int -> ST s (Int, Int, Int)
                    reconPix !px !py = do
                      !uvR <- interpChan bestUVr px py
                      !uvG <- interpChan bestUVg px py
                      !uvB <- interpChan bestUVb px py
                      !by <- fromIntegral <$> VSM.unsafeRead bestY (py * workW + px)
                      return (clip10 (by + uvR), clip10 (by + uvG), clip10 (by + uvB))
                (!r00, !g00, !b00) <- reconPix px0 py0
                (!r10, !g10, !b10) <- reconPix (px0 + 1) py0
                (!r01, !g01, !b01) <- reconPix px0 (py0 + 1)
                (!r11, !g11, !b11) <- reconPix (px0 + 1) (py0 + 1)
                let !rDown = scaleDown4 r00 r10 r01 r11
                    !gDown = scaleDown4 g00 g10 g01 g11
                    !bDown = scaleDown4 b00 b10 b01 b11
                    !wg = rgbToGray rDown gDown bDown
                    !idx = cy * uvW + cx
                VSM.unsafeWrite reconUVr idx $! fromIntegral (rDown - wg)
                VSM.unsafeWrite reconUVg idx $! fromIntegral (gDown - wg)
                VSM.unsafeWrite reconUVb idx $! fromIntegral (bDown - wg)

            -- Step B: Compute reconstructed Y for all pixels
            forM_ [0 .. workH - 1] $ \py ->
              forM_ [0 .. workW - 1] $ \px -> do
                !uvR <- interpChan bestUVr px py
                !uvG <- interpChan bestUVg px py
                !uvB <- interpChan bestUVb px py
                !by <- fromIntegral <$> VSM.unsafeRead bestY (py * workW + px)
                let !reconR = clip10 (by + uvR)
                    !reconG = clip10 (by + uvG)
                    !reconB = clip10 (by + uvB)
                    !reconY = linearToGamma (rgbToGray (gammaToLinear reconR) (gammaToLinear reconG) (gammaToLinear reconB))
                VSM.unsafeWrite reconYBuf (py * workW + px) (fromIntegral reconY)

            -- Step C: Update bestY (clipped) and accumulate diff
            let updateYLoop :: Int -> Int -> ST s Int
                updateYLoop !i !diffSum
                  | i >= pixSize = return diffSum
                  | otherwise = do
                      !tgt <- fromIntegral <$> VSM.unsafeRead targetY i
                      !rec <- fromIntegral <$> VSM.unsafeRead reconYBuf i
                      !by <- fromIntegral <$> VSM.unsafeRead bestY i
                      let !diff = tgt - rec
                          !newBy = clip10 (by + diff)
                      VSM.unsafeWrite bestY i (fromIntegral newBy)
                      updateYLoop (i + 1) (diffSum + abs diff)
            !diffSum <- updateYLoop 0 0

            -- Step D: Update bestUV (unclamped)
            let updateUVLoop :: VSM.MVector s Int16 -> VSM.MVector s Int16 -> VSM.MVector s Int16 -> Int -> ST s ()
                updateUVLoop !best !tgt !rec !i
                  | i >= uvSize = return ()
                  | otherwise = do
                      !tv <- fromIntegral <$> VSM.unsafeRead tgt i
                      !rv <- fromIntegral <$> VSM.unsafeRead rec i
                      !bv <- fromIntegral <$> VSM.unsafeRead best i
                      VSM.unsafeWrite best i (fromIntegral (bv + tv - rv))
                      updateUVLoop best tgt rec (i + 1)
            updateUVLoop bestUVr targetUVr reconUVr 0
            updateUVLoop bestUVg targetUVg reconUVg 0
            updateUVLoop bestUVb targetUVb reconUVb 0

            -- Step E: Convergence check
            let !threshold = 3 * workW * workH
            when (iter == 0 || (diffSum < threshold && diffSum < prevDiffSum)) $
              iterate_ (iter + 1) diffSum

  iterate_ 0 maxBound

  -- -----------------------------------------------------------------------
  -- Phase 3: Convert optimized (bestY, bestUV) to output Y/U/V
  -- -----------------------------------------------------------------------

  -- Y: upsample UV + bestY → reconstruct 10-bit RGB → BT.601 luma → 8-bit
  forM_ [0 .. h - 1] $ \py ->
    forM_ [0 .. w - 1] $ \px -> do
      !uvR <- interpChan bestUVr px py
      !uvG <- interpChan bestUVg px py
      !uvB <- interpChan bestUVb px py
      !by <- fromIntegral <$> VSM.unsafeRead bestY (py * workW + px)
      let !reconR = clip10 (by + uvR)
          !reconG = clip10 (by + uvG)
          !reconB = clip10 (by + uvB)
          -- (77R + 150G + 29B + 512) >> 10 is the 10-bit-scale BT.601 → 8-bit Y
          !y8 = clip255 $ (77 * reconR + 150 * reconG + 29 * reconB + 512) `shiftR` 10
      VSM.write yBuf (py * paddedW + px) y8

  -- U, V: compute directly from UV residuals (W cancels in chroma formulas)
  let !outChromaRows = (h + 1) `shiftR` 1
      !outChromaCols = (w + 1) `shiftR` 1
  forM_ [0 .. outChromaRows - 1] $ \cy ->
    forM_ [0 .. outChromaCols - 1] $ \cx -> do
      !uvr <- fromIntegral <$> VSM.unsafeRead bestUVr (cy * uvW + cx)
      !uvg <- fromIntegral <$> VSM.unsafeRead bestUVg (cy * uvW + cx)
      !uvb <- fromIntegral <$> VSM.unsafeRead bestUVb (cy * uvW + cx)
      let !u8 = clip255 $ ((-43 * uvr - 85 * uvg + 128 * uvb + 512) `shiftR` 10) + 128
          !v8 = clip255 $ ((128 * uvr - 107 * uvg - 21 * uvb + 512) `shiftR` 10) + 128
      VSM.write uBuf (cy * chromaW + cx) u8
      VSM.write vBuf (cy * chromaW + cx) v8

  return (yBuf, uBuf, vBuf)
