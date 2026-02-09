{-# LANGUAGE BangPatterns #-}

-- | Complete VP8L encoder with proper Huffman coding
module Codec.Picture.WebP.Internal.VP8L.EncodeComplete
  ( encodeVP8LComplete,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.BitWriter
import Codec.Picture.WebP.Internal.VP8L.PredictorEncode
import Codec.Picture.WebP.Internal.VP8L.SubresolutionEncode
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- | Maximum code length for VP8L Huffman codes
maxCodeLength :: Int
maxCodeLength = 15

-- | Complete VP8L encoder with proper Huffman coding
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

      -- Compute predictor transform (sizeBits=4 -> 16x16 blocks)
      -- Only use predictor transform for images large enough to benefit
      usePredictorTransform = width >= 8 && height >= 8
      sizeBits = 4

      (pixelsToEncode, maybePredResult) =
        if usePredictorTransform
          then
            let pr = computePredictorTransform sizeBits width height argbPixels
             in (prResiduals pr, Just pr)
          else (argbPixels, Nothing)

      -- Build histograms
      hists = buildHistograms pixelsToEncode

      -- Generate Huffman codes from histograms
      codes = generateHuffmanCodes hists

      -- Build the VP8L bitstream
      w =
        emptyBitWriter
          |> writeBits 8 0x2F -- VP8L signature
          |> writeBits 14 (fromIntegral $ width - 1)
          |> writeBits 14 (fromIntegral $ height - 1)
          |> writeBit True -- alpha_is_used
          |> writeBits 3 0 -- version (must be 0)
          |> writeTransformHeader maybePredResult sizeBits
          |> writeBit False -- no color cache
          |> writeBit False -- single prefix code group (no meta prefix)
          |> writeHuffmanCode (cGreen codes) 280 -- Green alphabet: 256 + 24 LZ77 length codes (no cache)
          |> writeHuffmanCode (cRed codes) 256 -- Red: 256 symbols
          |> writeHuffmanCode (cBlue codes) 256 -- Blue: 256 symbols
          |> writeHuffmanCode (cAlpha codes) 256 -- Alpha: 256 symbols
          |> writeHuffmanCode (cDist codes) 40 -- Distance: 40 symbols (not used, but required)
          |> encodePixels pixelsToEncode codes
          |> finalizeBitWriter
   in bitWriterToByteString w
  where
    (|>) = flip ($)

-- | Write transform header (predictor transform if enabled, otherwise just no-transform marker)
writeTransformHeader :: Maybe PredictorResult -> Int -> BitWriter -> BitWriter
writeTransformHeader Nothing _ w =
  writeBit False w -- no transforms
writeTransformHeader (Just predResult) sizeBits w =
  let w1 = writeBit True w -- transform_present = 1
      w2 = writeBits 2 0 w1 -- transform_type = 0 (predictor)
      w3 = writeBits 3 (fromIntegral sizeBits) w2 -- size_bits (decoder uses this directly)
      w4 =
        encodeSubresolutionImage
          (prTransformWidth predResult)
          (prTransformHeight predResult)
          (prModeImage predResult)
          w3
      w5 = writeBit False w4 -- no more transforms
   in w5

-- | Histogram data for all channels
data Histograms = Histograms
  { hGreen :: !(VU.Vector Int),
    hRed :: !(VU.Vector Int),
    hBlue :: !(VU.Vector Int),
    hAlpha :: !(VU.Vector Int)
  }

-- | Huffman codes for all channels
-- Each entry is (symbol, codeValue, codeLength)
data HuffmanCodes = HuffmanCodes
  { cGreen :: !(VU.Vector (Int, Word32, Int)),
    cRed :: !(VU.Vector (Int, Word32, Int)),
    cBlue :: !(VU.Vector (Int, Word32, Int)),
    cAlpha :: !(VU.Vector (Int, Word32, Int)),
    cDist :: !(VU.Vector (Int, Word32, Int)), -- Distance codes (all zero for literals only)
    -- Lookup tables for fast encoding (256 entries each)
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
{-# INLINE buildLookup #-}
buildLookup :: VU.Vector (Int, Word32, Int) -> VU.Vector (Word32, Int)
buildLookup codes = runST $ do
  lookup <- VUM.replicate 256 (0, 0)
  VU.forM_ codes $ \(sym, code, len) ->
    when (sym < 256) $
      VUM.write lookup sym (code, len)
  VU.unsafeFreeze lookup

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

-- | Compute length-limited Huffman code lengths
-- Uses a simplified approach: assigns lengths based on frequency ranking
-- with proper length limiting to maxCodeLength
{-# INLINE computeCodeLengths #-}
computeCodeLengths :: VU.Vector (Int, Int) -> VU.Vector (Int, Int)
computeCodeLengths symFreqs =
  let n = VU.length symFreqs
      -- Use Kraft inequality to assign valid lengths
      -- For n symbols, we need sum(2^-len_i) <= 1
      -- Simple strategy: use ceil(log2(n)) as base length, adjust for frequency
      baseLen = max 1 $ ceilLog2 n

      -- Assign lengths: more frequent symbols get shorter codes
      -- Sort by frequency descending and assign lengths using in-place sort
      sortedDesc = runST $ do
        mv <- VU.thaw symFreqs
        -- Sort descending by negating the comparison
        VA.sortBy (\(_, f1) (_, f2) -> compare f2 f1) mv
        VU.unsafeFreeze mv

      -- Assign lengths ensuring Kraft inequality
      withLengths = assignLengths sortedDesc baseLen
   in withLengths

-- | Assign code lengths respecting Kraft inequality and max length
-- Uses a simple approach: assign length ceil(log2(n)) to all symbols
-- This always satisfies Kraft inequality for n symbols
assignLengths :: VU.Vector (Int, Int) -> Int -> VU.Vector (Int, Int)
assignLengths symFreqs baseLen =
  let n = VU.length symFreqs
      -- For n symbols, we need ceil(log2(n)) bits per symbol
      -- This is suboptimal but always valid
      uniformLen = max 1 $ ceilLog2 n
      -- Ensure uniform length doesn't exceed max
      safeLen = min maxCodeLength uniformLen
   in VU.map (\(sym, _) -> (sym, safeLen)) symFreqs

-- | Adjust code lengths to satisfy Kraft inequality
adjustForKraft :: VU.Vector (Int, Int) -> VU.Vector (Int, Int)
adjustForKraft symLens =
  let n = VU.length symLens
      -- Calculate current Kraft sum (scaled by 2^maxCodeLength)
      kraftSum =
        VU.foldl'
          ( \acc (_, len) ->
              acc + (1 `shiftL` (maxCodeLength - len))
          )
          0
          symLens

      -- Target is 2^maxCodeLength (scaled sum should equal this)
      target = 1 `shiftL` maxCodeLength :: Int
   in -- If sum > target, increase lengths; if sum < target, we're OK
      -- (Kraft inequality allows sum <= 1)
      if kraftSum > target
        then -- Need to increase some lengths
          increaseUntilValid symLens kraftSum target
        else symLens

-- | Increase code lengths until Kraft inequality is satisfied
increaseUntilValid :: VU.Vector (Int, Int) -> Int -> Int -> VU.Vector (Int, Int)
increaseUntilValid symLens currentSum targetSum
  | currentSum <= targetSum = symLens
  | otherwise =
      -- Find the symbol with shortest length that can be increased
      let minLen = VU.minimum $ VU.map snd symLens
          -- Increase length of first symbol with minLen
          (updated, newSum) =
            VU.ifoldl'
              ( \(acc, csum) i (sym, len) ->
                  if len == minLen && csum > targetSum && len < maxCodeLength
                    then
                      let newLen = len + 1
                          delta = (1 `shiftL` (maxCodeLength - len)) - (1 `shiftL` (maxCodeLength - newLen))
                       in (VU.snoc acc (sym, newLen), csum - delta)
                    else (VU.snoc acc (sym, len), csum)
              )
              (VU.empty, currentSum)
              symLens
       in if newSum == currentSum
            then symLens -- Can't reduce further
            else increaseUntilValid updated newSum targetSum

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
  | VU.length codes == 1 =
      -- Single symbol
      let (sym, _, _) = codes VU.! 0
       in writeSimpleCode1 sym w
  | VU.length codes == 2 =
      -- Two symbols
      let (s1, _, _) = codes VU.! 0
          (s2, _, _) = codes VU.! 1
       in writeSimpleCode2 s1 s2 w
  | otherwise =
      -- 3+ symbols: use normal code length encoding
      writeNormalCode codes alphabetSize w

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

-- | Build code length code (CLC) symbols and lengths
-- Returns (run-length encoded symbols, CLC alphabet lengths)
buildCLC :: VU.Vector Int -> Int -> ([(Int, Int)], VU.Vector Int)
buildCLC codeLengths maxSym =
  let -- Run-length encode the code lengths
      rle = runLengthEncode $ VU.toList $ VU.take (maxSym + 1) codeLengths

      -- Count CLC symbol frequencies
      clcFreqs = runST $ do
        freqs <- VUM.replicate 19 (0 :: Int)
        mapM_ (\(sym, _) -> VUM.modify freqs (+ 1) sym) rle
        VU.unsafeFreeze freqs

      -- Assign CLC lengths (simple approach: use frequency-based lengths)
      clcLengths = assignClcLengths clcFreqs
   in (rle, clcLengths)

-- | Run-length encode code lengths using symbols 0-18
-- Simply output each code length as a literal (no run-length encoding for now)
-- This is simpler and guaranteed correct, though less compressed
runLengthEncode :: [Int] -> [(Int, Int)]
runLengthEncode = map (\len -> (len, 0))

-- | Assign CLC lengths based on frequencies
assignClcLengths :: VU.Vector Int -> VU.Vector Int
assignClcLengths freqs =
  let -- Count non-zero symbols
      numNonZero = VU.length $ VU.filter (> 0) freqs
   in if numNonZero <= 1
        then -- All zeros or single symbol: assign length 1 to all used
          VU.generate 19 $ \i -> if freqs VU.! i > 0 then 1 else 0
        else -- Multiple symbols: use frequency-based lengths
          VU.imap
            ( \_ f ->
                if f == 0
                  then 0
                  else min 7 $ max 1 $ 8 - ceilLog2 (max 1 f)
            )
            freqs

-- | Find how many CLC entries to write (minimum 4)
findNumClcToWrite :: VU.Vector Int -> [Int] -> Int
findNumClcToWrite clcLengths order =
  let -- Find last non-zero in order
      indexed = zip [0 ..] order
      lastNonZero =
        foldl
          ( \acc (i, sym) ->
              if clcLengths VU.! sym > 0 then i + 1 else acc
          )
          4
          indexed
   in max 4 lastNonZero

-- | Build canonical codes for CLC
buildClcCanonicalCodes :: VU.Vector Int -> VU.Vector (Word32, Int)
buildClcCanonicalCodes clcLengths = runST $ do
  -- Count at each length
  blCount <- VUM.replicate 8 (0 :: Int)
  VU.iforM_ clcLengths $ \_ len ->
    when (len > 0 && len <= 7) $
      VUM.modify blCount (+ 1) len

  -- Compute next_code
  nextCode <- VUM.replicate 8 (0 :: Int)
  code <- VUM.new 1
  VUM.write code 0 0

  forM_ [1 .. 7] $ \bits -> do
    c <- VUM.read code 0
    prevCount <- VUM.read blCount (bits - 1)
    let newCode = (c + prevCount) `shiftL` 1
    VUM.write nextCode bits newCode
    VUM.write code 0 newCode

  -- Assign codes
  result <- VUM.replicate 19 (0, 0)
  VU.iforM_ clcLengths $ \sym len ->
    when (len > 0) $ do
      c <- VUM.read nextCode len
      VUM.write nextCode len (c + 1)
      VUM.write result sym (fromIntegral c, len)

  VU.unsafeFreeze result

-- | Write code lengths using the CLC
writeCodeLengthsWithCLC :: VU.Vector Int -> Int -> [(Int, Int)] -> VU.Vector (Word32, Int) -> BitWriter -> BitWriter
writeCodeLengthsWithCLC _codeLengths _numSymbols rleSymbols clcCodes w =
  foldl
    ( \wa (sym, extra) ->
        let (code, len) = clcCodes VU.! sym
            wa' = writeBitsReversed len (fromIntegral code) wa
         in case sym of
              16 -> writeBits 2 (fromIntegral extra) wa' -- repeat: 2 extra bits
              17 -> writeBits 3 (fromIntegral extra) wa' -- short zero run: 3 extra bits
              18 -> writeBits 7 (fromIntegral extra) wa' -- long zero run: 7 extra bits
              _ -> wa' -- literal length: no extra bits
    )
    w
    rleSymbols

-- | Encode pixels using Huffman codes
{-# INLINE encodePixels #-}
encodePixels :: VS.Vector Word32 -> HuffmanCodes -> BitWriter -> BitWriter
encodePixels pixels codes w =
  let !greenLookup = lGreen codes
      !redLookup = lRed codes
      !blueLookup = lBlue codes
      !alphaLookup = lAlpha codes
   in VS.foldl'
        ( \wa px ->
            let !g = fromIntegral ((px `shiftR` 8) .&. 0xFF) :: Int
                !r = fromIntegral ((px `shiftR` 16) .&. 0xFF) :: Int
                !b = fromIntegral (px .&. 0xFF) :: Int
                !a = fromIntegral ((px `shiftR` 24) .&. 0xFF) :: Int

                -- Use unsafe indexing - indices are guaranteed valid (0-255 from 8-bit components)
                !gEntry = greenLookup `VU.unsafeIndex` g
                !rEntry = redLookup `VU.unsafeIndex` r
                !bEntry = blueLookup `VU.unsafeIndex` b
                !aEntry = alphaLookup `VU.unsafeIndex` a

                -- Only write bits if length > 0
                !wa1 = if snd gEntry > 0 then writeBitsReversed (snd gEntry) (fromIntegral (fst gEntry)) wa else wa
                !wa2 = if snd rEntry > 0 then writeBitsReversed (snd rEntry) (fromIntegral (fst rEntry)) wa1 else wa1
                !wa3 = if snd bEntry > 0 then writeBitsReversed (snd bEntry) (fromIntegral (fst bEntry)) wa2 else wa2
                !wa4 = if snd aEntry > 0 then writeBitsReversed (snd aEntry) (fromIntegral (fst aEntry)) wa3 else wa3
             in wa4
        )
        w
        pixels

-- Helper functions

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
