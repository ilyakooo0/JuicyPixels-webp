{-# LANGUAGE BangPatterns #-}

-- | Complete VP8L encoder with proper Huffman coding and LZ77 compression
module Codec.Picture.WebP.Internal.VP8L.EncodeComplete
  ( encodeVP8LComplete,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.BitWriter
import Codec.Picture.WebP.Internal.VP8L.ColorIndexingEncode
import Codec.Picture.WebP.Internal.VP8L.ColorTransformEncode
import Codec.Picture.WebP.Internal.VP8L.LZ77Encode
import Codec.Picture.WebP.Internal.VP8L.PredictorEncode
import Codec.Picture.WebP.Internal.VP8L.SubresolutionEncode
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IM
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- | Maximum code length for VP8L Huffman codes
maxCodeLength :: Int
maxCodeLength = 15

-- | Complete VP8L encoder with proper Huffman coding and LZ77 compression
encodeVP8LComplete :: Image PixelRGBA8 -> B.ByteString
encodeVP8LComplete img =
  let width = imageWidth img
      height = imageHeight img

      -- Convert to ARGB pixels
      !imgPixels = imageData img
      argbPixels = VS.generate (width * height) $ \i ->
        let !base = i * 4
            !r = imgPixels `VS.unsafeIndex` base
            !g = imgPixels `VS.unsafeIndex` (base + 1)
            !b = imgPixels `VS.unsafeIndex` (base + 2)
            !a = imgPixels `VS.unsafeIndex` (base + 3)
         in packARGB a r g b

      -- Try color-indexing transform (for images with ≤ 256 unique colors)
      maybeColorIndex = tryColorIndexing width height argbPixels

      -- Effective pixels and width after color indexing
      (effectivePixels, effectiveWidth) = case maybeColorIndex of
        Nothing -> (argbPixels, width)
        Just ci -> (ciIndexedPixels ci, ciPackedWidth ci)

      -- Compute predictor transform (sizeBits=4 -> 16x16 blocks)
      -- Only use predictor transform for images large enough to benefit
      usePredictorTransform = effectiveWidth >= 8 && height >= 8
      sizeBits = 4

      (pixelsToEncode, maybePredResult) =
        if usePredictorTransform
          then
            let pr = computePredictorTransform sizeBits effectiveWidth height effectivePixels
             in (prResiduals pr, Just pr)
          else (effectivePixels, Nothing)

      -- Channel decorrelation: use color transform (preferred) or subtract-green (fallback).
      -- Color transform is a strict generalization of subtract-green with per-block
      -- coefficients via least-squares regression. Use it when not using color indexing
      -- and image is large enough to justify the subresolution image overhead.
      useColorTransform = case maybeColorIndex of
        Nothing -> usePredictorTransform
        Just _ -> False

      useSubtractGreen = case maybeColorIndex of
        Nothing -> not useColorTransform
        Just _ -> False

      (pixelsForLZ77, maybeColorResult) =
        if useColorTransform
          then
            let cr = computeColorTransform sizeBits effectiveWidth height pixelsToEncode
             in (ctTransformedPixels cr, Just cr)
          else
            ( if useSubtractGreen
                then applyForwardSubtractGreen pixelsToEncode
                else pixelsToEncode,
              Nothing
            )

      -- LZ77 compress the pixel data (at effective width)
      tokens = lz77Compress effectiveWidth height pixelsForLZ77

      -- Build histograms from LZ77 tokens
      reverseDistMap = buildReverseDistanceMap effectiveWidth
      hists = buildHistogramsFromTokens reverseDistMap tokens

      -- Generate Huffman codes from extended histograms
      codes = generateHuffmanCodes hists

      -- Build the VP8L bitstream
      w =
        emptyBitWriter
          |> writeBits 8 0x2F -- VP8L signature
          |> writeBits 14 (fromIntegral $ width - 1) -- original width
          |> writeBits 14 (fromIntegral $ height - 1)
          |> writeBit True -- alpha_is_used
          |> writeBits 3 0 -- version (must be 0)
          |> writeAllTransforms maybeColorIndex maybePredResult maybeColorResult useSubtractGreen sizeBits
          |> writeBit False -- no color cache
          |> writeBit False -- single prefix code group (no meta prefix)
          |> writeHuffmanCode (cGreen codes) 280 -- Green alphabet: 256 + 24 LZ77 length codes (no cache)
          |> writeHuffmanCode (cRed codes) 256 -- Red: 256 symbols
          |> writeHuffmanCode (cBlue codes) 256 -- Blue: 256 symbols
          |> writeHuffmanCode (cAlpha codes) 256 -- Alpha: 256 symbols
          |> writeHuffmanCode (cDist codes) 40 -- Distance: 40 symbols
          |> encodeTokens reverseDistMap tokens codes
          |> finalizeBitWriter
   in bitWriterToByteString w
  where
    (|>) = flip ($)

-- | Write all transform headers (color-indexing, predictor, color/subtract-green, then no-more-transforms marker)
writeAllTransforms :: Maybe ColorIndexResult -> Maybe PredictorResult -> Maybe ColorTransformResult -> Bool -> Int -> BitWriter -> BitWriter
writeAllTransforms maybeCI maybePred maybeColor subGreen sizeBits w0 =
  let w1 = case maybeCI of
        Just ci -> writeColorIndexTransform ci w0
        Nothing -> w0
      w2 = case maybePred of
        Just pr -> writePredictorTransform pr sizeBits w1
        Nothing -> w1
      w3 = case maybeColor of
        Just cr -> writeColorTransform cr sizeBits w2
        Nothing ->
          if subGreen
            then writeSubtractGreenTransform w2
            else w2
   in writeBit False w3 -- no more transforms

-- | Write subtract-green transform header (type 2, no additional data)
writeSubtractGreenTransform :: BitWriter -> BitWriter
writeSubtractGreenTransform w =
  let w1 = writeBit True w -- transform_present = 1
      w2 = writeBits 2 2 w1 -- transform_type = 2 (subtract green)
   in w2

-- | Write color transform header (type 1)
writeColorTransform :: ColorTransformResult -> Int -> BitWriter -> BitWriter
writeColorTransform cr sizeBits w =
  let w1 = writeBit True w -- transform_present = 1
      w2 = writeBits 2 1 w1 -- transform_type = 1 (color)
      w3 = writeBits 3 (fromIntegral $ sizeBits - 2) w2 -- decoder reads ReadBits(3) + 2
      w4 =
        encodeSubresolutionImage
          (ctTransformWidth cr)
          (ctTransformHeight cr)
          (ctTransformImage cr)
          w3
   in w4

-- | Write color-indexing transform header
writeColorIndexTransform :: ColorIndexResult -> BitWriter -> BitWriter
writeColorIndexTransform ci w =
  let w1 = writeBit True w -- transform_present = 1
      w2 = writeBits 2 3 w1 -- transform_type = 3 (color indexing)
      w3 = writeBits 8 (fromIntegral $ ciPaletteSize ci - 1) w2 -- color_table_size - 1
      w4 = encodeSubresolutionImage (ciPaletteSize ci) 1 (ciPalette ci) w3
   in w4

-- | Write predictor transform header
writePredictorTransform :: PredictorResult -> Int -> BitWriter -> BitWriter
writePredictorTransform predResult sizeBits w =
  let w1 = writeBit True w -- transform_present = 1
      w2 = writeBits 2 0 w1 -- transform_type = 0 (predictor)
      w3 = writeBits 3 (fromIntegral $ sizeBits - 2) w2 -- decoder reads ReadBits(3) + 2
      w4 =
        encodeSubresolutionImage
          (prTransformWidth predResult)
          (prTransformHeight predResult)
          (prModeImage predResult)
          w3
   in w4

-- | Histogram data for all channels (extended for LZ77)
data Histograms = Histograms
  { hGreen :: !(VU.Vector Int), -- 280 entries: 256 literals + 24 length codes
    hRed :: !(VU.Vector Int), -- 256 entries
    hBlue :: !(VU.Vector Int), -- 256 entries
    hAlpha :: !(VU.Vector Int), -- 256 entries
    hDist :: !(VU.Vector Int) -- 40 entries
  }

-- | Huffman codes for all channels
-- Each entry is (symbol, codeValue, codeLength)
data HuffmanCodes = HuffmanCodes
  { cGreen :: !(VU.Vector (Int, Word32, Int)),
    cRed :: !(VU.Vector (Int, Word32, Int)),
    cBlue :: !(VU.Vector (Int, Word32, Int)),
    cAlpha :: !(VU.Vector (Int, Word32, Int)),
    cDist :: !(VU.Vector (Int, Word32, Int)),
    -- Lookup tables for fast encoding
    lGreen :: !(VU.Vector (Word32, Int)), -- 280 entries (indexed by green symbol)
    lRed :: !(VU.Vector (Word32, Int)), -- 256 entries
    lBlue :: !(VU.Vector (Word32, Int)), -- 256 entries
    lAlpha :: !(VU.Vector (Word32, Int)), -- 256 entries
    lDist :: !(VU.Vector (Word32, Int)) -- 40 entries (indexed by distance prefix code)
  }

-- | Build frequency histograms from LZ77 tokens
buildHistogramsFromTokens :: IM.IntMap Int -> V.Vector Token -> Histograms
buildHistogramsFromTokens reverseDistMap tokens = runST $ do
  gHist <- VUM.replicate 280 0
  rHist <- VUM.replicate 256 0
  bHist <- VUM.replicate 256 0
  aHist <- VUM.replicate 256 0
  dHist <- VUM.replicate 40 0

  V.forM_ tokens $ \tok -> case tok of
    TLiteral px -> do
      VUM.unsafeModify gHist (+ 1) (fromIntegral $ (px `shiftR` 8) .&. 0xFF)
      VUM.unsafeModify rHist (+ 1) (fromIntegral $ (px `shiftR` 16) .&. 0xFF)
      VUM.unsafeModify bHist (+ 1) (fromIntegral $ px .&. 0xFF)
      VUM.unsafeModify aHist (+ 1) (fromIntegral $ (px `shiftR` 24) .&. 0xFF)
    TBackRef len dist -> do
      -- Length prefix code -> green symbol 256..279
      let (!lenPC, _, _) = valueToPrefixCode len
      VUM.unsafeModify gHist (+ 1) (256 + lenPC)
      -- Distance -> distance code -> distance prefix code -> symbol 0..39
      let !distCode = distToDistCode reverseDistMap dist
          (!distPC, _, _) = valueToPrefixCode distCode
      VUM.unsafeModify dHist (+ 1) distPC

  g <- VU.unsafeFreeze gHist
  r <- VU.unsafeFreeze rHist
  b <- VU.unsafeFreeze bHist
  a <- VU.unsafeFreeze aHist
  d <- VU.unsafeFreeze dHist
  return $ Histograms g r b a d

-- | Convert a scan-line distance to a VP8L distance code.
{-# INLINE distToDistCode #-}
distToDistCode :: IM.IntMap Int -> Int -> Int
distToDistCode reverseMap dist =
  case IM.lookup dist reverseMap of
    Just code2d -> code2d
    Nothing -> dist + 120

-- | Generate Huffman codes from histograms
generateHuffmanCodes :: Histograms -> HuffmanCodes
generateHuffmanCodes hists =
  let gCodes = huffmanFromHistogram (hGreen hists)
      rCodes = huffmanFromHistogram (hRed hists)
      bCodes = huffmanFromHistogram (hBlue hists)
      aCodes = huffmanFromHistogram (hAlpha hists)
      dCodes = huffmanFromHistogram (hDist hists)
   in HuffmanCodes
        { cGreen = gCodes,
          cRed = rCodes,
          cBlue = bCodes,
          cAlpha = aCodes,
          cDist = dCodes,
          lGreen = buildLookup 280 gCodes,
          lRed = buildLookup 256 rCodes,
          lBlue = buildLookup 256 bCodes,
          lAlpha = buildLookup 256 aCodes,
          lDist = buildLookup 40 dCodes
        }

-- | Build lookup table from codes for fast encoding
{-# INLINE buildLookup #-}
buildLookup :: Int -> VU.Vector (Int, Word32, Int) -> VU.Vector (Word32, Int)
buildLookup size codes = runST $ do
  tbl <- VUM.replicate size (0, 0)
  VU.forM_ codes $ \(sym, code, len) ->
    when (sym < size) $
      VUM.write tbl sym (code, len)
  VU.unsafeFreeze tbl

-- | Encode LZ77 tokens using Huffman codes
encodeTokens :: IM.IntMap Int -> V.Vector Token -> HuffmanCodes -> BitWriter -> BitWriter
encodeTokens reverseDistMap tokens codes w0 =
  let !greenLookup = lGreen codes
      !redLookup = lRed codes
      !blueLookup = lBlue codes
      !alphaLookup = lAlpha codes
      !distLookup = lDist codes
   in V.foldl'
        ( \wa tok -> case tok of
            TLiteral px ->
              let !g = fromIntegral ((px `shiftR` 8) .&. 0xFF) :: Int
                  !r = fromIntegral ((px `shiftR` 16) .&. 0xFF) :: Int
                  !b = fromIntegral (px .&. 0xFF) :: Int
                  !a = fromIntegral ((px `shiftR` 24) .&. 0xFF) :: Int
                  !wa1 = writeHuffSym greenLookup g wa
                  !wa2 = writeHuffSym redLookup r wa1
                  !wa3 = writeHuffSym blueLookup b wa2
                  !wa4 = writeHuffSym alphaLookup a wa3
               in wa4
            TBackRef len dist ->
              let -- Encode length: prefix code goes into green symbol 256+
                  (!lenPC, !lenExtra, !lenExtraVal) = valueToPrefixCode len
                  !greenSym = 256 + lenPC
                  !wa1 = writeHuffSym greenLookup greenSym wa
                  !wa2 =
                    if lenExtra > 0
                      then writeBits lenExtra (fromIntegral lenExtraVal) wa1
                      else wa1
                  -- Encode distance: convert to distance code, then prefix code
                  !dc = distToDistCode reverseDistMap dist
                  (!distPC, !distExtra, !distExtraVal) = valueToPrefixCode dc
                  !wa3 = writeHuffSym distLookup distPC wa2
                  !wa4 =
                    if distExtra > 0
                      then writeBits distExtra (fromIntegral distExtraVal) wa3
                      else wa3
               in wa4
        )
        w0
        tokens
  where
    {-# INLINE writeHuffSym #-}
    writeHuffSym lut sym wa =
      let (!code, !len) = lut `VU.unsafeIndex` sym
       in if len > 0 then writeBitsReversed len (fromIntegral code) wa else wa

-- | Generate Huffman codes from a histogram
-- Returns vector of (symbol, codeValue, codeLength)
{-# INLINE huffmanFromHistogram #-}
huffmanFromHistogram :: VU.Vector Int -> VU.Vector (Int, Word32, Int)
huffmanFromHistogram hist =
  let -- Find symbols with non-zero frequency
      nonZeroSymbols = VU.findIndices (> 0) hist
      numSymbols = VU.length nonZeroSymbols
   in case numSymbols of
        0 -> VU.singleton (0, 0, 0) -- Empty: default to symbol 0, 0 bits needed
        1 -> VU.singleton (nonZeroSymbols VU.! 0, 0, 0) -- Single symbol: 0 bits needed
        2 ->
          -- Two symbols: both get length 1
          let s0 = nonZeroSymbols VU.! 0
              s1 = nonZeroSymbols VU.! 1
           in VU.fromList [(s0, 0, 1), (s1, 1, 1)]
        _ ->
          -- 3+ symbols: build proper Huffman tree
          buildHuffmanCodes nonZeroSymbols hist

-- | Build Huffman codes for 3+ symbols using length-limited Huffman
{-# INLINE buildHuffmanCodes #-}
buildHuffmanCodes :: VU.Vector Int -> VU.Vector Int -> VU.Vector (Int, Word32, Int)
buildHuffmanCodes symbols hist =
  let -- Get symbol frequencies
      symFreqs = VU.map (\sym -> (sym, hist VU.! sym)) symbols

      -- Sort by frequency (ascending) for Huffman tree building using in-place sort
      sortedSymFreqs = runST $ do
        mv <- VU.thaw symFreqs
        VA.sortBy (comparing snd) mv
        VU.unsafeFreeze mv

      -- Compute code lengths using length-limited Huffman
      codeLengths = computeCodeLengths sortedSymFreqs

      -- Build canonical codes from lengths
      canonicalCodes = buildCanonicalCodes codeLengths
   in canonicalCodes

-- | Compute Huffman code lengths using the standard Huffman tree algorithm.
-- Produces a COMPLETE code (Kraft sum = 1), required by VP8L.
-- Code lengths are limited to maxCodeLength (15) using the DEFLATE-style
-- length-limiting algorithm.
computeCodeLengths :: VU.Vector (Int, Int) -> VU.Vector (Int, Int)
computeCodeLengths symFreqs =
  let freqList = VU.toList symFreqs
      depths = huffmanDepths freqList
      maxD = maximum (map snd depths)
   in if maxD <= maxCodeLength
        then VU.fromList depths -- No clamping needed
        else VU.fromList (limitCodeLengths depths)

-- | Limit code lengths to maxCodeLength while maintaining a valid (Kraft sum = 1) code.
-- Uses the iterative algorithm from DEFLATE/zlib: when clamping creates oversubscription,
-- repeatedly promote the deepest symbol below maxCodeLength to free up code space.
limitCodeLengths :: [(Int, Int)] -> [(Int, Int)]
limitCodeLengths depths =
  let -- Sort by depth descending (deepest first), then by symbol
      sorted = sortBy (\(_, d1) (_, d2) -> compare d2 d1) depths
      -- Clamp all depths to maxCodeLength
      clamped = map (\(s, d) -> (s, min maxCodeLength (max 1 d))) sorted
      -- Compute Kraft sum in units of 2^(-maxCodeLength)
      -- For a valid code: sum must equal 2^maxCodeLength = 32768
      target = 1 `shiftL` maxCodeLength :: Int
      kraftSum xs = sum [1 `shiftL` (maxCodeLength - d) | (_, d) <- xs]
      excess = kraftSum clamped - target
   in if excess <= 0
        then clamped -- Already valid (shouldn't happen if max depth > 15)
        else fixOversubscribed clamped excess

-- | Fix an oversubscribed code by lengthening short codes.
-- Works from the deepest non-maxCodeLength codes toward shallower ones.
-- The input list must be sorted descending by depth.
fixOversubscribed :: [(Int, Int)] -> Int -> [(Int, Int)]
fixOversubscribed syms excess = go syms excess
  where
    go xs 0 = xs
    go xs ex
      | ex < 0 = xs -- Slightly undersubscribed is acceptable
      | otherwise =
          -- List is sorted descending. Find the first (deepest) symbol < maxCodeLength.
          let (atMax, rest) = break (\(_, d) -> d < maxCodeLength) xs
           in case rest of
                [] -> xs -- All at maxCodeLength, can't fix
                ((s, d) : after) ->
                  let newLen = d + 1
                      freed = (1 `shiftL` (maxCodeLength - d)) - (1 `shiftL` (maxCodeLength - newLen))
                      newList = atMax ++ ((s, newLen) : after)
                   in go newList (ex - freed)

-- | Build a Huffman tree from (symbol, frequency) pairs and return (symbol, depth) pairs.
-- Uses a simple list-based priority queue (sufficient for alphabet sizes up to ~300).
huffmanDepths :: [(Int, Int)] -> [(Int, Int)]
huffmanDepths [] = []
huffmanDepths [(sym, _)] = [(sym, 1)]
huffmanDepths pairs =
  let -- Create initial leaves sorted by frequency
      sorted = sortBy (comparing snd) pairs
      leaves = map (\(s, f) -> HLeaf s f) sorted
      -- Build tree by repeatedly merging two lowest-frequency nodes
      tree = buildTree leaves
   in -- Extract symbol depths from tree
      treeToDepths 0 tree

-- | Huffman tree data type
data HTree = HLeaf !Int !Int | HNode !Int HTree HTree

htreeFreq :: HTree -> Int
htreeFreq (HLeaf _ f) = f
htreeFreq (HNode f _ _) = f

-- | Build Huffman tree from sorted list of tree nodes
buildTree :: [HTree] -> HTree
buildTree [t] = t
buildTree (t1 : t2 : rest) =
  let merged = HNode (htreeFreq t1 + htreeFreq t2) t1 t2
   in buildTree (insertByFreq merged rest)
buildTree [] = HLeaf 0 0 -- shouldn't happen

-- | Insert a tree node into a frequency-sorted list
insertByFreq :: HTree -> [HTree] -> [HTree]
insertByFreq node [] = [node]
insertByFreq node (x : xs)
  | htreeFreq node <= htreeFreq x = node : x : xs
  | otherwise = x : insertByFreq node xs

-- | Extract (symbol, depth) pairs from tree
treeToDepths :: Int -> HTree -> [(Int, Int)]
treeToDepths depth (HLeaf sym _) = [(sym, max 1 depth)]
treeToDepths depth (HNode _ l r) =
  treeToDepths (depth + 1) l ++ treeToDepths (depth + 1) r

-- | Build canonical Huffman codes from symbol-length pairs
buildCanonicalCodes :: VU.Vector (Int, Int) -> VU.Vector (Int, Word32, Int)
buildCanonicalCodes symLens = runST $ do
  -- Count symbols at each code length
  blCount <- VUM.replicate (maxCodeLength + 1) (0 :: Int)
  VU.forM_ symLens $ \(_, len) ->
    when (len > 0 && len <= maxCodeLength) $
      VUM.modify blCount (+ 1) len

  -- Compute starting code values for each length
  nextCode <- VUM.replicate (maxCodeLength + 1) (0 :: Int)
  code <- VUM.new 1
  VUM.write code 0 0

  forM_ [1 .. maxCodeLength] $ \bits -> do
    c <- VUM.read code 0
    prevCount <- VUM.read blCount (bits - 1)
    let newCode = (c + prevCount) `shiftL` 1
    VUM.write nextCode bits newCode
    VUM.write code 0 newCode

  -- Assign codes in symbol order (for canonical ordering)
  -- First sort by (length, symbol) for canonical ordering
  let sorted = sortBy (\(s1, l1) (s2, l2) -> compare (l1, s1) (l2, s2)) $ VU.toList symLens

  result <- VUM.new (length sorted)

  forM_ (zip [0 ..] sorted) $ \(i, (sym, len)) -> do
    c <- VUM.read nextCode len
    VUM.write nextCode len (c + 1)
    VUM.write result i (sym, fromIntegral c, len)

  VU.unsafeFreeze result

-- | Write a Huffman code tree to the bitstream
-- alphabetSize is the maximum number of symbols in this alphabet
writeHuffmanCode :: VU.Vector (Int, Word32, Int) -> Int -> BitWriter -> BitWriter
writeHuffmanCode codes alphabetSize w
  | VU.null codes =
      -- Empty: encode as single symbol 0
      writeSimpleCode1 0 w
  | VU.length codes == 1 && maxSym <= 255 =
      -- Single symbol that fits in simple code
      writeSimpleCode1 maxSym w
  | VU.length codes == 2 && maxSym <= 255 =
      -- Two symbols that both fit in simple code (8-bit max)
      let (s1, _, _) = codes VU.! 0
          (s2, _, _) = codes VU.! 1
       in writeSimpleCode2 s1 s2 w
  | otherwise =
      -- 3+ symbols or symbols > 255: use normal code length encoding
      writeNormalCode codes alphabetSize w
  where
    maxSym = if VU.null codes then 0 else VU.maximum $ VU.map (\(s, _, _) -> s) codes

-- | Write simple code for 1 symbol
{-# INLINE writeSimpleCode1 #-}
writeSimpleCode1 :: Int -> BitWriter -> BitWriter
writeSimpleCode1 sym w =
  let isFirst8Bits = sym > 1
      numBits = if isFirst8Bits then 8 else 1
   in w
        |> writeBit True -- is_simple = 1
        |> writeBit False -- num_symbols - 1 = 0
        |> writeBit isFirst8Bits -- is_first_8bits
        |> writeBits numBits (fromIntegral sym)
  where
    (|>) = flip ($)

-- | Write simple code for 2 symbols
{-# INLINE writeSimpleCode2 #-}
writeSimpleCode2 :: Int -> Int -> BitWriter -> BitWriter
writeSimpleCode2 s1 s2 w =
  let isFirst8Bits = s1 > 1
      numBitsFirst = if isFirst8Bits then 8 else 1
   in w
        |> writeBit True -- is_simple = 1
        |> writeBit True -- num_symbols - 1 = 1
        |> writeBit isFirst8Bits -- is_first_8bits
        |> writeBits numBitsFirst (fromIntegral s1)
        |> writeBits 8 (fromIntegral s2)
  where
    (|>) = flip ($)

-- | Write normal (non-simple) Huffman code
-- This uses the two-level code length code (CLC) scheme
-- Simplified version: use fixed 4-bit CLC for symbols 0-15
writeNormalCode :: VU.Vector (Int, Word32, Int) -> Int -> BitWriter -> BitWriter
writeNormalCode codes alphabetSize w =
  let -- Build code length array (0 = symbol not present)
      codeLengthArray = buildCodeLengthArray codes alphabetSize

      -- Find max symbol used (add 1 because decoder reads symbols 0 to max_symbol-1)
      maxSymbol = findMaxSymbol codes + 1

      -- Use a simple fixed CLC: all symbols 0-15 get length 4
      -- This gives us 16 possible code length values (0-15), encoded with 4 bits each
      -- kCodeLengthCodeOrder = [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
      -- We need symbols 0-15 for literal code lengths, so write up to position 18 (which includes 15)
      numClcToWrite = 19 -- All 19 positions to ensure we cover symbols 0-15
      w1 = writeBit False w -- is_simple = 0

      -- Write number of CLC lengths
      w2 = writeBits 4 (fromIntegral $ numClcToWrite - 4) w1

      -- Write CLC lengths: 4 bits for symbols 0-15, 0 for symbols 16-18
      clcOrder = [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
      w3 =
        foldl
          ( \wa i ->
              let clcSym = clcOrder !! i
                  -- Symbols 0-15 get length 4, symbols 16-18 get length 0
                  clcLen = if clcSym <= 15 then 4 else 0
               in writeBits 3 (fromIntegral clcLen) wa
          )
          w2
          [0 .. numClcToWrite - 1]

      -- Build canonical codes for the fixed CLC
      -- With 16 symbols all having length 4, they get codes 0..15
      fixedClcCodes = VU.generate 19 $ \sym ->
        if sym <= 15
          then (fromIntegral sym :: Word32, 4 :: Int) -- symbol N gets code N, length 4
          else (0, 0) -- symbols 16-18 not used

      -- Write use_max_symbol and max_symbol
      w4 = writeBit True w3 -- use_max_symbol = 1
      maxSymValue = max 2 maxSymbol
      valueToEncode = maxSymValue - 2
      bitsNeeded = if valueToEncode <= 0 then 1 else ceilLog2 (valueToEncode + 1)
      lengthNbits = max 2 $ ((bitsNeeded + 1) `div` 2) * 2
      w5 = writeBits 3 (fromIntegral $ (lengthNbits - 2) `div` 2) w4
      w6 = writeBits lengthNbits (fromIntegral valueToEncode) w5

      -- Write code lengths directly using fixed 4-bit codes
      -- Each code length 0-15 is encoded as its own value with 4 bits
      -- Decoder reads symbols 0 to max_symbol-1, so we write sym < maxSymbol
      w7 =
        VU.ifoldl'
          ( \wa sym len ->
              if sym < maxSymbol
                then
                  let (code, codeLen) = fixedClcCodes VU.! len
                   in writeBitsReversed codeLen (fromIntegral code) wa
                else wa
          )
          w6
          codeLengthArray
   in w7

-- | Build code length array from codes
{-# INLINE buildCodeLengthArray #-}
buildCodeLengthArray :: VU.Vector (Int, Word32, Int) -> Int -> VU.Vector Int
buildCodeLengthArray codes size = runST $ do
  arr <- VUM.replicate size 0
  VU.forM_ codes $ \(sym, _, len) ->
    when (sym < size) $
      VUM.write arr sym len
  VU.unsafeFreeze arr

-- | Find maximum symbol in codes
{-# INLINE findMaxSymbol #-}
findMaxSymbol :: VU.Vector (Int, Word32, Int) -> Int
findMaxSymbol codes = VU.maximum $ VU.map (\(sym, _, _) -> sym) codes

-- Helper functions

-- | Forward subtract-green transform: subtract green from red and blue channels.
-- Inverse of the decoder's inverseSubtractGreen (which adds green back).
applyForwardSubtractGreen :: VS.Vector Word32 -> VS.Vector Word32
applyForwardSubtractGreen = VS.map subtractGreenPixel
  where
    {-# INLINE subtractGreenPixel #-}
    subtractGreenPixel px =
      let !g = (px `shiftR` 8) .&. 0xFF
          !r = (px `shiftR` 16) .&. 0xFF
          !b = px .&. 0xFF
          !r' = (r - g) .&. 0xFF
          !b' = (b - g) .&. 0xFF
       in (px .&. 0xFF00FF00) .|. (r' `shiftL` 16) .|. b'

{-# INLINE packARGB #-}
packARGB :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
packARGB a r g b =
  (fromIntegral a `shiftL` 24)
    .|. (fromIntegral r `shiftL` 16)
    .|. (fromIntegral g `shiftL` 8)
    .|. fromIntegral b

{-# INLINE ceilLog2 #-}
ceilLog2 :: Int -> Int
ceilLog2 n
  | n <= 1 = 0
  | n <= 2 = 1
  | n <= 4 = 2
  | n <= 8 = 3
  | n <= 16 = 4
  | n <= 32 = 5
  | n <= 64 = 6
  | n <= 128 = 7
  | n <= 256 = 8
  | otherwise = 9

when :: (Monad m) => Bool -> m () -> m ()
when True action = action
when False _ = return ()

forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ xs f = sequence_ (map f xs)
