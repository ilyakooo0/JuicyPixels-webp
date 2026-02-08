{-# LANGUAGE BangPatterns #-}

-- | VP8L subresolution image encoder (for transform data)
module Codec.Picture.WebP.Internal.VP8L.SubresolutionEncode
  ( encodeSubresolutionImage,
  )
where

import Codec.Picture.WebP.Internal.BitWriter
import Control.Monad.ST
import Data.Bits
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- | Maximum code length for VP8L Huffman codes
maxCodeLength :: Int
maxCodeLength = 15

-- | Encode a subresolution image (mode map for predictor transform, etc.)
-- Subresolution images have NO transform bits - they start directly with color cache bit
encodeSubresolutionImage :: Int -> Int -> VS.Vector Word32 -> BitWriter -> BitWriter
encodeSubresolutionImage width height pixels w =
  let -- Build histograms for each channel
      hists = buildHistograms pixels

      -- Generate Huffman codes from histograms
      codes = generateHuffmanCodes hists

      -- Encode subresolution image:
      -- - no transform_present bit (subresolution images don't have transforms)
      -- - color_cache_used (1 bit) = 0
      -- - NO use_meta_prefix bit for subresolution images!
      -- - prefix_code_group (5 Huffman codes)
      -- - entropy_coded_pixels
      w1 = writeBit False w -- no color cache
      -- NOTE: subresolution images do NOT have the use_meta_prefix bit!
      w3 = writeHuffmanCode (cGreen codes) 280 w1 -- Green alphabet
      w4 = writeHuffmanCode (cRed codes) 256 w3 -- Red
      w5 = writeHuffmanCode (cBlue codes) 256 w4 -- Blue
      w6 = writeHuffmanCode (cAlpha codes) 256 w5 -- Alpha
      w7 = writeHuffmanCode (cDist codes) 40 w6 -- Distance
      w8 = encodePixels pixels codes w7
   in w8

-- | Histogram data for all channels
data Histograms = Histograms
  { hGreen :: !(VU.Vector Int),
    hRed :: !(VU.Vector Int),
    hBlue :: !(VU.Vector Int),
    hAlpha :: !(VU.Vector Int)
  }

-- | Huffman codes for all channels
data HuffmanCodes = HuffmanCodes
  { cGreen :: !(VU.Vector (Int, Word32, Int)),
    cRed :: !(VU.Vector (Int, Word32, Int)),
    cBlue :: !(VU.Vector (Int, Word32, Int)),
    cAlpha :: !(VU.Vector (Int, Word32, Int)),
    cDist :: !(VU.Vector (Int, Word32, Int)),
    -- Lookup tables for fast encoding
    lGreen :: !(VU.Vector (Word32, Int)),
    lRed :: !(VU.Vector (Word32, Int)),
    lBlue :: !(VU.Vector (Word32, Int)),
    lAlpha :: !(VU.Vector (Word32, Int))
  }

-- | Build frequency histograms for each channel
buildHistograms :: VS.Vector Word32 -> Histograms
buildHistograms pixels = runST $ do
  gHist <- VUM.replicate 256 0
  rHist <- VUM.replicate 256 0
  bHist <- VUM.replicate 256 0
  aHist <- VUM.replicate 256 0

  VS.forM_ pixels $ \px -> do
    VUM.modify gHist (+ 1) (fromIntegral $ (px `shiftR` 8) .&. 0xFF)
    VUM.modify rHist (+ 1) (fromIntegral $ (px `shiftR` 16) .&. 0xFF)
    VUM.modify bHist (+ 1) (fromIntegral $ px .&. 0xFF)
    VUM.modify aHist (+ 1) (fromIntegral $ (px `shiftR` 24) .&. 0xFF)

  g <- VU.unsafeFreeze gHist
  r <- VU.unsafeFreeze rHist
  b <- VU.unsafeFreeze bHist
  a <- VU.unsafeFreeze aHist
  return $ Histograms g r b a

-- | Generate Huffman codes from histograms
generateHuffmanCodes :: Histograms -> HuffmanCodes
generateHuffmanCodes hists =
  let gCodes = huffmanFromHistogram (hGreen hists)
      rCodes = huffmanFromHistogram (hRed hists)
      bCodes = huffmanFromHistogram (hBlue hists)
      aCodes = huffmanFromHistogram (hAlpha hists)
      -- Distance codes: single symbol 0 (no backward references used)
      dCodes = VU.singleton (0, 0, 1)
   in HuffmanCodes
        { cGreen = gCodes,
          cRed = rCodes,
          cBlue = bCodes,
          cAlpha = aCodes,
          cDist = dCodes,
          lGreen = buildLookup gCodes,
          lRed = buildLookup rCodes,
          lBlue = buildLookup bCodes,
          lAlpha = buildLookup aCodes
        }

-- | Build lookup table from codes for fast encoding
buildLookup :: VU.Vector (Int, Word32, Int) -> VU.Vector (Word32, Int)
buildLookup codes = runST $ do
  tbl <- VUM.replicate 256 (0, 0)
  VU.forM_ codes $ \(sym, code, len) ->
    when (sym < 256) $
      VUM.write tbl sym (code, len)
  VU.unsafeFreeze tbl

-- | Generate Huffman codes from a histogram
huffmanFromHistogram :: VU.Vector Int -> VU.Vector (Int, Word32, Int)
huffmanFromHistogram hist =
  let -- Find symbols with non-zero frequency
      nonZeroSymbols = VU.ifilter (\i _ -> hist VU.! i > 0) (VU.enumFromN 0 (VU.length hist))
      numSymbols = VU.length nonZeroSymbols
   in case numSymbols of
        0 -> VU.singleton (0, 0, 0)
        1 -> VU.singleton (nonZeroSymbols VU.! 0, 0, 0)
        2 ->
          let s0 = nonZeroSymbols VU.! 0
              s1 = nonZeroSymbols VU.! 1
           in VU.fromList [(s0, 0, 1), (s1, 1, 1)]
        _ -> buildHuffmanCodes nonZeroSymbols hist

-- | Build Huffman codes for 3+ symbols
buildHuffmanCodes :: VU.Vector Int -> VU.Vector Int -> VU.Vector (Int, Word32, Int)
buildHuffmanCodes symbols hist =
  let symFreqs = VU.map (\sym -> (sym, hist VU.! sym)) symbols
      sortedSymFreqs = VU.fromList $ sortBy (comparing snd) $ VU.toList symFreqs
      codeLengths = computeCodeLengths sortedSymFreqs
   in buildCanonicalCodes codeLengths

-- | Compute length-limited Huffman code lengths
computeCodeLengths :: VU.Vector (Int, Int) -> VU.Vector (Int, Int)
computeCodeLengths symFreqs =
  let n = VU.length symFreqs
      baseLen = max 1 $ ceilLog2 n
      sortedDesc = VU.fromList $ reverse $ sortBy (comparing snd) $ VU.toList symFreqs
   in assignLengths sortedDesc baseLen

-- | Assign code lengths
assignLengths :: VU.Vector (Int, Int) -> Int -> VU.Vector (Int, Int)
assignLengths symFreqs _baseLen =
  let n = VU.length symFreqs
      uniformLen = max 1 $ ceilLog2 n
      safeLen = min maxCodeLength uniformLen
   in VU.map (\(sym, _) -> (sym, safeLen)) symFreqs

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

  -- Assign codes in canonical order
  let sorted = sortBy (\(s1, l1) (s2, l2) -> compare (l1, s1) (l2, s2)) $ VU.toList symLens

  result <- VUM.new (length sorted)

  forM_ (zip [0 ..] sorted) $ \(i, (sym, len)) -> do
    c <- VUM.read nextCode len
    VUM.write nextCode len (c + 1)
    VUM.write result i (sym, fromIntegral c, len)

  VU.unsafeFreeze result

-- | Write a Huffman code tree to the bitstream
writeHuffmanCode :: VU.Vector (Int, Word32, Int) -> Int -> BitWriter -> BitWriter
writeHuffmanCode codes alphabetSize w
  | VU.null codes = writeSimpleCode1 0 w
  | VU.length codes == 1 =
      let (sym, _, _) = codes VU.! 0
       in writeSimpleCode1 sym w
  | VU.length codes == 2 =
      let (s1, _, _) = codes VU.! 0
          (s2, _, _) = codes VU.! 1
       in writeSimpleCode2 s1 s2 w
  | otherwise = writeNormalCode codes alphabetSize w

-- | Write simple code for 1 symbol
writeSimpleCode1 :: Int -> BitWriter -> BitWriter
writeSimpleCode1 sym w =
  let isFirst8Bits = sym > 1
      numBits = if isFirst8Bits then 8 else 1
      w1 = writeBit True w -- is_simple = 1
      w2 = writeBit False w1 -- num_symbols - 1 = 0
      w3 = writeBit isFirst8Bits w2 -- is_first_8bits
      w4 = writeBits numBits (fromIntegral sym) w3
   in w4

-- | Write simple code for 2 symbols
writeSimpleCode2 :: Int -> Int -> BitWriter -> BitWriter
writeSimpleCode2 s1 s2 w =
  let isFirst8Bits = s1 > 1
      numBitsFirst = if isFirst8Bits then 8 else 1
      w1 = writeBit True w -- is_simple = 1
      w2 = writeBit True w1 -- num_symbols - 1 = 1
      w3 = writeBit isFirst8Bits w2 -- is_first_8bits
      w4 = writeBits numBitsFirst (fromIntegral s1) w3
      w5 = writeBits 8 (fromIntegral s2) w4
   in w5

-- | Write normal (non-simple) Huffman code
writeNormalCode :: VU.Vector (Int, Word32, Int) -> Int -> BitWriter -> BitWriter
writeNormalCode codes alphabetSize w =
  let codeLengthArray = buildCodeLengthArray codes alphabetSize
      maxSymbol = findMaxSymbol codes + 1
      numClcToWrite = 19
      w1 = writeBit False w -- is_simple = 0
      w2 = writeBits 4 (fromIntegral $ numClcToWrite - 4) w1

      clcOrder = [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
      w3 =
        foldl
          ( \wa i ->
              let clcSym = clcOrder !! i
                  clcLen = if clcSym <= 15 then 4 else 0
               in writeBits 3 (fromIntegral clcLen) wa
          )
          w2
          [0 .. numClcToWrite - 1]

      fixedClcCodes = VU.generate 19 $ \sym ->
        if sym <= 15
          then (fromIntegral sym :: Word32, 4 :: Int)
          else (0, 0)

      w4 = writeBit True w3 -- use_max_symbol = 1
      maxSymValue = max 2 maxSymbol
      valueToEncode = maxSymValue - 2
      bitsNeeded = if valueToEncode <= 0 then 1 else ceilLog2 (valueToEncode + 1)
      lengthNbits = max 2 $ ((bitsNeeded + 1) `div` 2) * 2
      w5 = writeBits 3 (fromIntegral $ (lengthNbits - 2) `div` 2) w4
      w6 = writeBits lengthNbits (fromIntegral valueToEncode) w5

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
buildCodeLengthArray :: VU.Vector (Int, Word32, Int) -> Int -> VU.Vector Int
buildCodeLengthArray codes size = runST $ do
  arr <- VUM.replicate size 0
  VU.forM_ codes $ \(sym, _, len) ->
    when (sym < size) $
      VUM.write arr sym len
  VU.unsafeFreeze arr

-- | Find maximum symbol in codes
findMaxSymbol :: VU.Vector (Int, Word32, Int) -> Int
findMaxSymbol codes = VU.maximum $ VU.map (\(sym, _, _) -> sym) codes

-- | Encode pixels using Huffman codes
encodePixels :: VS.Vector Word32 -> HuffmanCodes -> BitWriter -> BitWriter
encodePixels pixels codes w =
  VS.foldl'
    ( \wa px ->
        let g = fromIntegral ((px `shiftR` 8) .&. 0xFF) :: Int
            r = fromIntegral ((px `shiftR` 16) .&. 0xFF) :: Int
            b = fromIntegral (px .&. 0xFF) :: Int
            a = fromIntegral ((px `shiftR` 24) .&. 0xFF) :: Int

            gEntry = lGreen codes VU.! g
            rEntry = lRed codes VU.! r
            bEntry = lBlue codes VU.! b
            aEntry = lAlpha codes VU.! a

            wa1 = if snd gEntry > 0 then writeBitsReversed (snd gEntry) (fromIntegral (fst gEntry)) wa else wa
            wa2 = if snd rEntry > 0 then writeBitsReversed (snd rEntry) (fromIntegral (fst rEntry)) wa1 else wa1
            wa3 = if snd bEntry > 0 then writeBitsReversed (snd bEntry) (fromIntegral (fst bEntry)) wa2 else wa2
            wa4 = if snd aEntry > 0 then writeBitsReversed (snd aEntry) (fromIntegral (fst aEntry)) wa3 else wa3
         in wa4
    )
    w
    pixels

-- Helper functions

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
