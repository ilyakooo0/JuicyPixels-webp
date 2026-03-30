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
  Maybe (SegmentInfo, Word8, Word8, Word8) -> -- Segment info + 3 tree probs (Nothing = disabled)
  VU.Vector Word8 -> -- Coefficient probabilities (1056 entries, possibly updated)
  VU.Vector Bool -> -- Which positions to update (1056 flags)
  Maybe Word8 -> -- Skip mode: Just probSkipFalse to enable, Nothing to disable
  BoolEncoder
generateCompressedHeader quantIndices filterLevel filterType mSegInfo updatedProbs updateFlags mSkipProb =
  let enc0 = initBoolEncoder

      -- Color space and clamping (key frame only)
      enc1 = boolWriteLiteral 1 0 enc0 -- color_space = 0 (YCbCr BT.601)
      enc2 = boolWriteLiteral 1 0 enc1 -- clamping_type = 0 (clamping required)

      -- Segmentation
      enc3 = case mSegInfo of
        Nothing -> boolWriteLiteral 1 0 enc2 -- segmentation_enabled = 0
        Just (segInfo, sp0, sp1, sp2) ->
          encodeSegmentationHeader segInfo sp0 sp1 sp2 enc2

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
      -- 4 block types × 8 bands × 3 contexts × 11 tokens = 1056 probabilities
      enc16 = writeCoeffProbUpdates updatedProbs updateFlags enc15

      -- Macroblock skip mode
      -- mb_no_skip_coeff: 0 = skip mode disabled (all MBs have coefficients)
      --                   1 = skip mode enabled (must read prob_skip_false byte)
      enc17 = case mSkipProb of
        Nothing ->
          boolWriteLiteral 1 0 enc16 -- mb_no_skip_coeff = 0 (disabled)
        Just probSkipFalse ->
          let e1 = boolWriteLiteral 1 1 enc16 -- mb_no_skip_coeff = 1 (enabled)
           in boolWriteLiteral 8 (fromIntegral probSkipFalse) e1 -- prob_skip_false
   in enc17

-- | Write coefficient probability updates to the compressed header.
-- For each of the 1056 positions, writes a flag (using coeffUpdateProbs)
-- indicating whether the probability is updated. If updated, writes the
-- new 8-bit probability value as a literal.
writeCoeffProbUpdates :: VU.Vector Word8 -> VU.Vector Bool -> BoolEncoder -> BoolEncoder
writeCoeffProbUpdates updatedProbs updateFlags enc =
  let loop !i !j !k !l !e
        | i >= 4 = e
        | j >= 8 = loop (i + 1) 0 k l e
        | k >= 3 = loop i (j + 1) 0 l e
        | l >= 11 = loop i j (k + 1) 0 e
        | otherwise =
            let idx = i * 264 + j * 33 + k * 11 + l
                updateProb = coeffUpdateProbs VU.! idx
             in if updateFlags VU.! idx
                  then
                    -- Signal update: True flag + 8-bit probability value
                    let e1 = boolWrite updateProb True e
                        e2 = boolWriteLiteral 8 (fromIntegral $ updatedProbs VU.! idx) e1
                     in loop i j k (l + 1) e2
                  else
                    -- No update: False flag
                    let e1 = boolWrite updateProb False e
                     in loop i j k (l + 1) e1
   in loop 0 0 0 0 enc

-- | Encode the full segmentation header into the compressed header.
-- Layout per RFC 6386 Section 9.3:
--   segmentation_enabled L(1) = 1
--   update_mb_segmentation_map L(1) = 1
--   update_segment_feature_data L(1) = 1
--   segment_feature_mode L(1) = 0 (delta mode, per reference decoder)
--   4 quantizer deltas: present L(1), if present: magnitude L(7) + sign L(1)
--   4 filter deltas: present L(1), if present: magnitude L(6) + sign L(1)
--   3 segment tree probabilities: present L(1), prob L(8)
encodeSegmentationHeader :: SegmentInfo -> Word8 -> Word8 -> Word8 -> BoolEncoder -> BoolEncoder
encodeSegmentationHeader segInfo sp0 sp1 sp2 enc =
  let -- segmentation_enabled = 1
      enc1 = boolWriteLiteral 1 1 enc
      -- update_mb_segmentation_map = 1 (always for keyframes)
      enc2 = boolWriteLiteral 1 1 enc1
      -- update_segment_feature_data = 1
      enc3 = boolWriteLiteral 1 1 enc2
      -- segment_feature_mode = 0 (delta mode)
      enc4 = boolWriteLiteral 1 0 enc3

      -- 4 quantizer deltas
      enc5 = writeSegQuantizer (segmentQuantizer segInfo VU.! 0) enc4
      enc6 = writeSegQuantizer (segmentQuantizer segInfo VU.! 1) enc5
      enc7 = writeSegQuantizer (segmentQuantizer segInfo VU.! 2) enc6
      enc8 = writeSegQuantizer (segmentQuantizer segInfo VU.! 3) enc7

      -- 4 filter strength deltas
      enc9 = writeSegFilter (segmentFilterStrength segInfo VU.! 0) enc8
      enc10 = writeSegFilter (segmentFilterStrength segInfo VU.! 1) enc9
      enc11 = writeSegFilter (segmentFilterStrength segInfo VU.! 2) enc10
      enc12 = writeSegFilter (segmentFilterStrength segInfo VU.! 3) enc11

      -- 3 segment tree probabilities (always signal update)
      enc13 = boolWriteLiteral 1 1 enc12
      enc14 = boolWriteLiteral 8 (fromIntegral sp0) enc13
      enc15 = boolWriteLiteral 1 1 enc14
      enc16 = boolWriteLiteral 8 (fromIntegral sp1) enc15
      enc17 = boolWriteLiteral 1 1 enc16
      enc18 = boolWriteLiteral 8 (fromIntegral sp2) enc17
   in enc18

-- | Write a segment quantizer delta: present flag + signed 7-bit value
writeSegQuantizer :: Int -> BoolEncoder -> BoolEncoder
writeSegQuantizer 0 enc = boolWriteLiteral 1 0 enc -- not present
writeSegQuantizer delta enc =
  let enc1 = boolWriteLiteral 1 1 enc -- present
      enc2 = boolWriteLiteral 7 (fromIntegral (abs delta)) enc1 -- magnitude
      enc3 = boolWriteLiteral 1 (if delta < 0 then 1 else 0) enc2 -- sign
   in enc3

-- | Write a segment filter strength delta: present flag + signed 6-bit value
writeSegFilter :: Int -> BoolEncoder -> BoolEncoder
writeSegFilter 0 enc = boolWriteLiteral 1 0 enc -- not present
writeSegFilter delta enc =
  let enc1 = boolWriteLiteral 1 1 enc -- present
      enc2 = boolWriteLiteral 6 (fromIntegral (abs delta)) enc1 -- magnitude
      enc3 = boolWriteLiteral 1 (if delta < 0 then 1 else 0) enc2 -- sign
   in enc3
