{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.Header
  ( VP8FrameHeader (..),
    FilterDeltas (..),
    parseVP8Header,
  )
where

import Codec.Picture.WebP.Internal.VP8.BoolDecoder
import Codec.Picture.WebP.Internal.VP8.Dequant
import Codec.Picture.WebP.Internal.VP8.Tables
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- | Filter delta values
data FilterDeltas = FilterDeltas
  { fdRefLfDelta :: !(VU.Vector Int),
    fdModeLfDelta :: !(VU.Vector Int)
  }
  deriving (Show, Eq)

-- | VP8 frame header
data VP8FrameHeader = VP8FrameHeader
  { vp8Width :: !Int,
    vp8Height :: !Int,
    vp8HScale :: !Int,
    vp8VScale :: !Int,
    vp8ColorSpace :: !Int,
    vp8ClampingReq :: !Bool,
    vp8Segments :: !(Maybe SegmentInfo),
    vp8FilterType :: !Int,
    vp8FilterLevel :: !Int,
    vp8Sharpness :: !Int,
    vp8FilterDeltas :: !(Maybe FilterDeltas),
    vp8NumDCTPartitions :: !Int,
    vp8QuantIndices :: !QuantIndices,
    vp8CoeffProbs :: !(VU.Vector Word8),
    vp8SkipEnabled :: !Bool,
    vp8ProbSkipFalse :: !Word8,
    vp8FirstPartition :: !B.ByteString,
    vp8DCTPartitions :: ![B.ByteString],
    vp8Decoder :: !BoolDecoder -- BoolDecoder state after compressed header
  }
  deriving (Show)

-- | Parse VP8 frame header
parseVP8Header :: B.ByteString -> Either String VP8FrameHeader
parseVP8Header bs = do
  case runGetOrFail parseUncompressedHeader (fromStrict bs) of
    Left (_, _, err) -> Left $ "Failed to parse uncompressed header: " ++ err
    Right (rest, _, (w, h, hscale, vscale, partitionSize)) -> do
      let partitionBytes = B.take partitionSize (toStrict rest)
      let decoder = initBoolDecoder partitionBytes

      let (colorSpace, d1) = boolRead 128 decoder
          (clampingType, d2) = boolRead 128 d1

      (segments, d3) <- parseSegmentation d2

      let (filterType, d4) = boolRead 128 d3
          (filterLevelBits, d5) = boolLiteral 6 d4
          filterLevel = fromIntegral filterLevelBits
          (sharpnessBits, d6) = boolLiteral 3 d5
          sharpness = fromIntegral sharpnessBits

      (filterDeltas, d7) <- parseFilterDeltas d6

      let (log2Partitions, d8) = boolLiteral 2 d7
          numPartitions = 1 `shiftL` fromIntegral log2Partitions

      (quantIndices, d9) <- parseQuantIndices d8

      let (refreshEntropyProbs, d10) = boolRead 128 d9

      (coeffProbs, d11) <- parseCoeffProbs d10

      let (mbNoSkipCoeff, d12) = boolRead 128 d11

      (skipEnabled, probSkip, d13) <-
        if mbNoSkipCoeff
          then do
            let (prob, d) = boolLiteral 8 d12
            return (True, fromIntegral prob, d)
          else return (False, 0, d12)

      let remainingBytes = B.drop partitionSize (toStrict rest)
      dctPartitions <- parseDCTPartitions remainingBytes (numPartitions - 1)

      return $
        VP8FrameHeader
          { vp8Width = w,
            vp8Height = h,
            vp8HScale = hscale,
            vp8VScale = vscale,
            vp8ColorSpace = if colorSpace then 1 else 0,
            vp8ClampingReq = clampingType,
            vp8Segments = segments,
            vp8FilterType = if filterType then 1 else 0,
            vp8FilterLevel = filterLevel,
            vp8Sharpness = sharpness,
            vp8FilterDeltas = filterDeltas,
            vp8NumDCTPartitions = numPartitions,
            vp8QuantIndices = quantIndices,
            vp8CoeffProbs = coeffProbs,
            vp8SkipEnabled = skipEnabled,
            vp8ProbSkipFalse = probSkip,
            vp8FirstPartition = partitionBytes,
            vp8DCTPartitions = dctPartitions,
            vp8Decoder = d13 -- Pass the decoder state after compressed header
          }
  where
    fromStrict = B.fromStrict
    toStrict = B.toStrict

-- | Parse uncompressed header (10 bytes)
parseUncompressedHeader :: Get (Int, Int, Int, Int, Int)
parseUncompressedHeader = do
  byte0 <- getWord8
  byte1 <- getWord8
  byte2 <- getWord8

  let frameTag = byte0 .&. 0x01
      versionNum = (byte0 `shiftR` 1) .&. 0x07
      showFrame = (byte0 `shiftR` 4) .&. 0x01

  when (frameTag /= 0) $
    fail "Only keyframes are supported"

  startCode <- getByteString 3
  when (startCode /= B.pack [0x9D, 0x01, 0x2A]) $
    fail "Invalid VP8 start code"

  size0 <- getWord8
  size1 <- getWord8

  let widthField = fromIntegral size0 .|. ((fromIntegral size1 .&. 0x3F) `shiftL` 8)
      hscale = fromIntegral (size1 `shiftR` 6)

  size2 <- getWord8
  size3 <- getWord8

  let heightField = fromIntegral size2 .|. ((fromIntegral size3 .&. 0x3F) `shiftL` 8)
      vscale = fromIntegral (size3 `shiftR` 6)

  let width = widthField .&. 0x3FFF
      height = heightField .&. 0x3FFF

  -- Frame tag is 24-bit little-endian: frame_tag = byte0 | (byte1 << 8) | (byte2 << 16)
  -- first_part_size is bits [23:5] (19 bits): (frame_tag >> 5) & 0x7FFFF
  let frameTag24 = fromIntegral byte0 .|. (fromIntegral byte1 `shiftL` 8) .|. (fromIntegral byte2 `shiftL` 16) :: Int
      partitionSize = (frameTag24 `shiftR` 5) .&. 0x7FFFF

  return (width, height, hscale, vscale, partitionSize)

-- | Parse segmentation info
parseSegmentation :: BoolDecoder -> Either String (Maybe SegmentInfo, BoolDecoder)
parseSegmentation decoder = do
  let (enabled, d1) = boolRead 128 decoder

  if not enabled
    then return (Nothing, d1)
    else do
      let (updateMap, d2) = boolRead 128 d1
          (updateData, d3) = boolRead 128 d2

      (absoluteMode, quantizers, d4) <-
        if updateData
          then do
            let (absMode, d) = boolRead 128 d3
            (quants, d') <- parseSegmentQuantizers d
            return (absMode, quants, d')
          else return (False, VU.replicate 4 0, d3)

      (filterStrengths, d5) <-
        if updateData
          then parseSegmentFilterStrengths d4
          else return (VU.replicate 4 0, d4)

      let info =
            SegmentInfo
              { segmentEnabled = enabled,
                segmentUpdateMap = updateMap,
                segmentAbsoluteMode = absoluteMode,
                segmentQuantizer = quantizers,
                segmentFilterStrength = filterStrengths
              }

      return (Just info, d5)

-- | Parse segment quantizers
parseSegmentQuantizers :: BoolDecoder -> Either String (VU.Vector Int, BoolDecoder)
parseSegmentQuantizers decoder = do
  let loop !acc !i !d
        | i >= 4 = return (VU.fromList (reverse acc), d)
        | otherwise = do
            let (hasValue, d1) = boolRead 128 d
            if hasValue
              then do
                let (value, d2) = boolLiteral 7 d1
                    (sign, d3) = boolRead 128 d2
                    signedValue = if sign then -fromIntegral value else fromIntegral value
                loop (signedValue : acc) (i + 1) d3
              else loop (0 : acc) (i + 1) d1

  loop [] 0 decoder

-- | Parse segment filter strengths
parseSegmentFilterStrengths :: BoolDecoder -> Either String (VU.Vector Int, BoolDecoder)
parseSegmentFilterStrengths decoder = do
  let loop !acc !i !d
        | i >= 4 = return (VU.fromList (reverse acc), d)
        | otherwise = do
            let (hasValue, d1) = boolRead 128 d
            if hasValue
              then do
                let (value, d2) = boolLiteral 6 d1
                    (sign, d3) = boolRead 128 d2
                    signedValue = if sign then -fromIntegral value else fromIntegral value
                loop (signedValue : acc) (i + 1) d3
              else loop (0 : acc) (i + 1) d1

  loop [] 0 decoder

-- | Parse filter deltas
parseFilterDeltas :: BoolDecoder -> Either String (Maybe FilterDeltas, BoolDecoder)
parseFilterDeltas decoder = do
  let (enabled, d1) = boolRead 128 decoder

  if not enabled
    then return (Nothing, d1)
    else do
      let (updateData, d2) = boolRead 128 d1

      (refDeltas, d3) <-
        if updateData
          then parseRefLfDeltas d2
          else return (VU.replicate 4 0, d2)

      (modeDeltas, d4) <-
        if updateData
          then parseModeLfDeltas d3
          else return (VU.replicate 4 0, d3)

      let deltas = FilterDeltas refDeltas modeDeltas
      return (Just deltas, d4)

-- | Parse reference frame loop filter deltas
parseRefLfDeltas :: BoolDecoder -> Either String (VU.Vector Int, BoolDecoder)
parseRefLfDeltas decoder = do
  let loop !acc !i !d
        | i >= 4 = return (VU.fromList (reverse acc), d)
        | otherwise = do
            let (hasValue, d1) = boolRead 128 d
            if hasValue
              then do
                let (value, d2) = boolLiteral 6 d1
                    (sign, d3) = boolRead 128 d2
                    signedValue = if sign then -fromIntegral value else fromIntegral value
                loop (signedValue : acc) (i + 1) d3
              else loop (0 : acc) (i + 1) d1

  loop [] 0 decoder

-- | Parse mode loop filter deltas
parseModeLfDeltas :: BoolDecoder -> Either String (VU.Vector Int, BoolDecoder)
parseModeLfDeltas decoder = do
  let loop !acc !i !d
        | i >= 4 = return (VU.fromList (reverse acc), d)
        | otherwise = do
            let (hasValue, d1) = boolRead 128 d
            if hasValue
              then do
                let (value, d2) = boolLiteral 6 d1
                    (sign, d3) = boolRead 128 d2
                    signedValue = if sign then -fromIntegral value else fromIntegral value
                loop (signedValue : acc) (i + 1) d3
              else loop (0 : acc) (i + 1) d1

  loop [] 0 decoder

-- | Parse quantization indices
parseQuantIndices :: BoolDecoder -> Either String (QuantIndices, BoolDecoder)
parseQuantIndices decoder = do
  let (yacQiBits, d1) = boolLiteral 7 decoder
      yacQi = fromIntegral yacQiBits

  let readDelta d = do
        let (hasValue, d1) = boolRead 128 d
        if hasValue
          then do
            let (value, d2) = boolLiteral 4 d1
                (sign, d3) = boolRead 128 d2
            return (if sign then -fromIntegral value else fromIntegral value, d3)
          else return (0, d1)

  (ydcDelta, d2) <- readDelta d1
  (y2dcDelta, d3) <- readDelta d2
  (y2acDelta, d4) <- readDelta d3
  (uvdcDelta, d5) <- readDelta d4
  (uvacDelta, d6) <- readDelta d5

  let qi =
        QuantIndices
          { qiYacQi = yacQi,
            qiYdcDelta = ydcDelta,
            qiY2dcDelta = y2dcDelta,
            qiY2acDelta = y2acDelta,
            qiUvdcDelta = uvdcDelta,
            qiUvacDelta = uvacDelta
          }

  return (qi, d6)

-- | Parse coefficient probability updates
parseCoeffProbs :: BoolDecoder -> Either String (VU.Vector Word8, BoolDecoder)
parseCoeffProbs decoder = Right $ runST $ do
  probs <- VU.thaw defaultCoeffProbs

  let loop !d !i !j !k !l
        | i >= 4 = do
            p <- VU.unsafeFreeze probs
            return (p, d)
        | j >= 8 = loop d (i + 1) 0 k l
        | k >= 3 = loop d i (j + 1) 0 l
        | l >= 11 = loop d i j (k + 1) 0
        | otherwise = do
            let idx = i * 264 + j * 33 + k * 11 + l
                updateProb = coeffUpdateProbs VU.! idx
                (hasUpdate, d1) = boolRead updateProb d

            if hasUpdate
              then do
                let (newProb, d2) = boolLiteral 8 d1
                VUM.write probs idx (fromIntegral newProb)
                loop d2 i j k (l + 1)
              else loop d1 i j k (l + 1)

  loop decoder 0 0 0 0

-- | Parse DCT partition sizes
parseDCTPartitions :: B.ByteString -> Int -> Either String [B.ByteString]
parseDCTPartitions bs numPartitions
  | numPartitions == 0 = return []
  | otherwise = do
      case runGetOrFail (parsePartitionSizes numPartitions) (B.fromStrict bs) of
        Left (_, _, err) -> Left $ "Failed to parse DCT partition sizes: " ++ err
        Right (rest, _, sizes) -> do
          let partitions = extractPartitions (B.toStrict rest) sizes
          return partitions

-- | Parse partition sizes (3 bytes each, little-endian)
parsePartitionSizes :: Int -> Get [Int]
parsePartitionSizes n = mapM (const parsePartitionSize) [1 .. n]
  where
    parsePartitionSize = do
      b0 <- getWord8
      b1 <- getWord8
      b2 <- getWord8
      return $ fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8) .|. (fromIntegral b2 `shiftL` 16)

-- | Extract partitions from data
extractPartitions :: B.ByteString -> [Int] -> [B.ByteString]
extractPartitions bs sizes =
  let (parts, _) =
        foldl
          ( \(acc, remaining) size ->
              let part = B.take size remaining
                  rest = B.drop size remaining
               in (part : acc, rest)
          )
          ([], bs)
          sizes
   in reverse parts ++ [B.drop (sum sizes) bs]
