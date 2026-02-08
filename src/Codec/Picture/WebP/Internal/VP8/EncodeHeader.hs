{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.EncodeHeader
  ( generateUncompressedHeader,
    generateCompressedHeader,
  )
where

import Codec.Picture.WebP.Internal.VP8.BoolEncoder
import Codec.Picture.WebP.Internal.VP8.Dequant
import Codec.Picture.WebP.Internal.VP8.Tables
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Generate uncompressed VP8 frame header (10 bytes)
-- Returns the header bytes
generateUncompressedHeader ::
  Int -> -- Width
  Int -> -- Height
  Int -> -- First partition size (compressed header + mode data size)
  B.ByteString
generateUncompressedHeader width height firstPartSize =
  let -- Frame tag (3 bytes)
      frameType = 0 :: Word32 -- 0 = key frame
      version = 0 :: Word32 -- Version 0
      showFrame = 1 :: Word32 -- Show frame
      firstPartSize' = fromIntegral firstPartSize :: Word32

      -- Construct 3-byte frame tag (little-endian)
      -- Bit 0: frame_type
      -- Bits 1-3: version
      -- Bit 4: show_frame
      -- Bits 5-23: first_part_size (19 bits)
      tag =
        (frameType .&. 1)
          .|. ((version .&. 7) `shiftL` 1)
          .|. ((showFrame .&. 1) `shiftL` 4)
          .|. ((firstPartSize' .&. 0x7FFFF) `shiftL` 5)

      byte0 = fromIntegral (tag .&. 0xFF) :: Word8
      byte1 = fromIntegral ((tag `shiftR` 8) .&. 0xFF) :: Word8
      byte2 = fromIntegral ((tag `shiftR` 16) .&. 0xFF) :: Word8

      -- Key frame header (7 bytes)
      -- Bytes 3-5: Start code (0x9D 0x01 0x2A)
      startCode = B.pack [0x9D, 0x01, 0x2A]

      -- Bytes 6-7: Width (14 bits) + horizontal scale (2 bits)
      width' = fromIntegral width :: Word16
      hscale = 0 :: Word16 -- No scaling
      widthAndScale = (width' .&. 0x3FFF) .|. ((hscale .&. 3) `shiftL` 14)

      -- Bytes 8-9: Height (14 bits) + vertical scale (2 bits)
      height' = fromIntegral height :: Word16
      vscale = 0 :: Word16 -- No scaling
      heightAndScale = (height' .&. 0x3FFF) .|. ((vscale .&. 3) `shiftL` 14)

      -- Combine all parts
      frameTag = B.pack [byte0, byte1, byte2]
      dimensions = BL.toStrict $ runPut $ do
        putWord16le widthAndScale
        putWord16le heightAndScale
   in frameTag <> startCode <> dimensions

-- | Generate compressed VP8 frame header
-- Returns the encoded boolean data
generateCompressedHeader ::
  QuantIndices -> -- Quantization indices
  Int -> -- Filter level (0-63)
  Int -> -- Filter type (0=normal, 1=simple)
  BoolEncoder
generateCompressedHeader quantIndices filterLevel filterType =
  let enc0 = initBoolEncoder

      -- Color space and clamping (key frame only)
      enc1 = boolWriteLiteral 1 0 enc0 -- color_space = 0 (YCbCr BT.601)
      enc2 = boolWriteLiteral 1 0 enc1 -- clamping_type = 0 (clamping required)

      -- Segmentation
      enc3 = boolWriteLiteral 1 0 enc2 -- segmentation_enabled = 0

      -- Filter type and parameters
      enc4 = boolWriteLiteral 1 (fromIntegral filterType) enc3 -- filter_type
      enc5 = boolWriteLiteral 6 (fromIntegral filterLevel) enc4 -- loop_filter_level (0-63)
      enc6 = boolWriteLiteral 3 0 enc5 -- sharpness_level = 0

      -- Mode reference loop filter delta
      enc7 = boolWriteLiteral 1 0 enc6 -- mode_ref_lf_delta_enabled = 0

      -- Number of DCT partitions
      enc8 = boolWriteLiteral 2 0 enc7 -- log2_nbr_of_dct_partitions = 0 (1 partition)

      -- Quantization indices
      enc9 = boolWriteLiteral 7 (fromIntegral $ qiYacQi quantIndices) enc8 -- base_qi (0-127)

      -- Delta flags (all 0 for simple encoder)
      enc10 = boolWriteLiteral 1 0 enc9 -- y_dc_delta_present = 0
      enc11 = boolWriteLiteral 1 0 enc10 -- y2_dc_delta_present = 0
      enc12 = boolWriteLiteral 1 0 enc11 -- y2_ac_delta_present = 0
      enc13 = boolWriteLiteral 1 0 enc12 -- uv_dc_delta_present = 0
      enc14 = boolWriteLiteral 1 0 enc13 -- uv_ac_delta_present = 0

      -- Refresh entropy probabilities
      enc15 = boolWriteLiteral 1 1 enc14 -- refresh_entropy_probs = 1 (use defaults)

      -- Coefficient probability updates
      -- For simple encoder, don't update probabilities (write 0 for all update flags)
      -- There are 4 block types × 8 bands × 3 contexts × 11 tokens = 1056 probabilities
      -- Each has an update flag, so we need to write 1056 bits of 0
      enc16 = writeCoeffProbUpdates enc15

      -- Macroblock skip mode
      -- mb_no_skip_coeff: 0 = skip mode disabled (all MBs have coefficients)
      --                   1 = skip mode enabled (must read prob_skip_false byte)
      -- For simple encoder, disable skip mode (don't read per-MB skip flags)
      enc17 = boolWriteLiteral 1 0 enc16 -- mb_no_skip_coeff = 0 (skip mode disabled)
   in enc17

-- | Write coefficient probability updates (all zeros for simple encoder)
-- IMPORTANT: Must use the same probabilities as the decoder (coeffUpdateProbs)
writeCoeffProbUpdates :: BoolEncoder -> BoolEncoder
writeCoeffProbUpdates enc =
  -- 4 block types × 8 bands × 3 contexts × 11 tokens = 1056 update flags
  -- For simple encoder, write False (0) for all (don't update, use defaults)
  -- The decoder reads each flag using coeffUpdateProbs[idx], so we must write with the same probabilities
  let loop !i !j !k !l !e
        | i >= 4 = e
        | j >= 8 = loop (i + 1) 0 k l e
        | k >= 3 = loop i (j + 1) 0 l e
        | l >= 11 = loop i j (k + 1) 0 e
        | otherwise =
            let idx = i * 264 + j * 33 + k * 11 + l
                updateProb = coeffUpdateProbs VU.! idx
                e' = boolWrite updateProb False e -- write False (no update)
             in loop i j k (l + 1) e'
   in loop 0 0 0 0 enc
