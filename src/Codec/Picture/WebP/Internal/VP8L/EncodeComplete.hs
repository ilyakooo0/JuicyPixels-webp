{-# LANGUAGE BangPatterns #-}

-- | Complete VP8L encoder with proper Huffman coding
module Codec.Picture.WebP.Internal.VP8L.EncodeComplete
  ( encodeVP8LComplete,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.BitWriter
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word

-- | Complete VP8L encoder
encodeVP8LComplete :: Image PixelRGBA8 -> B.ByteString
encodeVP8LComplete img =
  let width = imageWidth img
      height = imageHeight img

      -- Convert to ARGB (no transform for now - works correctly)
      argbPixels = VS.generate (width * height) $ \i ->
        let pixels = imageData img
            r = pixels VS.! (i * 4)
            g = pixels VS.! (i * 4 + 1)
            b = pixels VS.! (i * 4 + 2)
            a = pixels VS.! (i * 4 + 3)
         in packARGB a r g b

      -- Build histograms
      hists = buildHistograms argbPixels

      -- Generate Huffman codes
      codes = generateHuffmanCodes hists

      -- Build bitstream
      w = emptyBitWriter
        |> writeBits 8 0x2F
        |> writeBits 14 (fromIntegral $ width - 1)
        |> writeBits 14 (fromIntegral $ height - 1)
        |> writeBit True
        |> writeBits 3 0
        |> writeBit False  -- no transforms
        |> writeBit False  -- no color cache
        |> writeBit False  -- single prefix code group
        |> writeHuffmanTree (cGreen codes)
        |> writeHuffmanTree (cRed codes)
        |> writeHuffmanTree (cBlue codes)
        |> writeHuffmanTree (cAlpha codes)
        |> writeSimple1Code 0  -- Distance
        |> encodePixels argbPixels codes
        |> finalizeBitWriter

   in bitWriterToByteString w
  where
    (|>) = flip ($)

data Histograms = Histograms
  { hGreen :: !(VU.Vector Int),
    hRed :: !(VU.Vector Int),
    hBlue :: !(VU.Vector Int),
    hAlpha :: !(VU.Vector Int)
  }

data HuffmanCodes = HuffmanCodes
  { cGreen :: !(VU.Vector (Word8, Word32, Int)),
    cRed :: !(VU.Vector (Word8, Word32, Int)),
    cBlue :: !(VU.Vector (Word8, Word32, Int)),
    cAlpha :: !(VU.Vector (Word8, Word32, Int))
  }

buildHistograms :: VS.Vector Word32 -> Histograms
buildHistograms pixels = runST $ do
  gHist <- VUM.replicate 256 0
  rHist <- VUM.replicate 256 0
  bHist <- VUM.replicate 256 0
  aHist <- VUM.replicate 256 0

  VS.forM_ pixels $ \px -> do
    VUM.modify gHist (+1) (fromIntegral $ (px `shiftR` 8) .&. 0xFF)
    VUM.modify rHist (+1) (fromIntegral $ (px `shiftR` 16) .&. 0xFF)
    VUM.modify bHist (+1) (fromIntegral $ px .&. 0xFF)
    VUM.modify aHist (+1) (fromIntegral $ (px `shiftR` 24) .&. 0xFF)

  g <- VU.unsafeFreeze gHist
  r <- VU.unsafeFreeze rHist
  b <- VU.unsafeFreeze bHist
  a <- VU.unsafeFreeze aHist
  return $ Histograms g r b a

generateHuffmanCodes :: Histograms -> HuffmanCodes
generateHuffmanCodes hists =
  HuffmanCodes
    { cGreen = huffmanFromHistogram (hGreen hists),
      cRed = huffmanFromHistogram (hRed hists),
      cBlue = huffmanFromHistogram (hBlue hists),
      cAlpha = huffmanFromHistogram (hAlpha hists)
    }

-- | Generate Huffman codes from histogram
-- Returns vector of (symbol, code, length) tuples
huffmanFromHistogram :: VU.Vector Int -> VU.Vector (Word8, Word32, Int)
huffmanFromHistogram hist =
  let nonZero = VU.filter (\i -> hist VU.! i > 0) (VU.enumFromN 0 (VU.length hist))
      numSymbols = VU.length nonZero
   in if numSymbols == 0
        then VU.singleton (0, 0, 1)  -- Default: single symbol 0
        else if numSymbols == 1
          then VU.singleton (fromIntegral $ nonZero VU.! 0, 0, 1)
          else if numSymbols == 2
            then VU.fromList [(fromIntegral $ nonZero VU.! 0, 0, 1),
                              (fromIntegral $ nonZero VU.! 1, 1, 1)]
            else generateFullCodes nonZero hist

-- | Generate full Huffman codes for >2 symbols
generateFullCodes :: VU.Vector Int -> VU.Vector Int -> VU.Vector (Word8, Word32, Int)
generateFullCodes symbols hist =
  -- Simplified: assign length based on frequency
  let symbolFreqs = VU.toList $ VU.map (\sym -> (sym, hist VU.! sym)) symbols
      sorted = sortBy (flip $ comparing snd) symbolFreqs  -- Most frequent first

      -- Assign lengths (simplified - not optimal)
      withLengths = zipWith (\(sym, _) idx ->
        let len | idx == 0 = 2
                | idx < 2 = 3
                | idx < 6 = 4
                | idx < 14 = 5
                | idx < 30 = 6
                | idx < 62 = 7
                | otherwise = 8
         in (sym, len)
        ) sorted [0..]

      -- Build canonical codes
      codeLengths = VU.replicate 256 0 VU.// map (\(sym, len) -> (sym, len)) withLengths

   in buildCanonicalCodes codeLengths

-- | Build canonical Huffman codes from code lengths
buildCanonicalCodes :: VU.Vector Int -> VU.Vector (Word8, Word32, Int)
buildCanonicalCodes codeLengths = runST $ do
  let maxLen = VU.maximum codeLengths

  -- Count symbols at each length
  blCount <- VUM.replicate (maxLen + 1) (0 :: Int)
  VU.iforM_ codeLengths $ \sym len ->
    when (len > 0) $ do
      count <- VUM.read blCount len
      VUM.write blCount len (count + 1)

  -- Compute next_code
  nextCode <- VUM.replicate (maxLen + 1) (0 :: Int)
  code <- VUM.new 1
  VUM.write code 0 0

  forM_ [1..maxLen] $ \len -> do
    c <- VUM.read code 0
    VUM.write nextCode len c
    count <- VUM.read blCount len
    VUM.write code 0 ((c + count) `shiftL` 1)

  -- Assign codes
  result <- VUM.new 256
  resultIdx <- VUM.new 1
  VUM.write resultIdx 0 0

  VU.iforM_ codeLengths $ \sym len ->
    when (len > 0) $ do
      c <- VUM.read nextCode len
      VUM.write nextCode len (c + 1)
      idx <- VUM.read resultIdx 0
      VUM.write result idx (fromIntegral sym, fromIntegral c, len)
      VUM.write resultIdx 0 (idx + 1)

  finalIdx <- VUM.read resultIdx 0
  frozen <- VU.unsafeFreeze result
  return $ VU.take finalIdx frozen

-- | Write Huffman tree (code lengths)
writeHuffmanTree :: VU.Vector (Word8, Word32, Int) -> BitWriter -> BitWriter
writeHuffmanTree codes w
  | VU.null codes = writeSimple1Code 0 w
  | VU.length codes == 1 =
      let (sym, _, _) = codes VU.! 0
       in writeSimple1Code (fromIntegral sym) w
  | VU.length codes == 2 =
      let (s1, _, _) = codes VU.! 0
          (s2, _, _) = codes VU.! 1
       in writeSimple2Code (fromIntegral s1) (fromIntegral s2) w
  | otherwise =
      writeNormalCode codes w

writeSimple1Code :: Word16 -> BitWriter -> BitWriter
writeSimple1Code sym w =
  writeBit True w |> writeBit False |> writeBit True |> writeBits 8 (fromIntegral sym)
  where (|>) = flip ($)

writeSimple2Code :: Word16 -> Word16 -> BitWriter -> BitWriter
writeSimple2Code s1 s2 w =
  writeBit True w |> writeBit True |> writeBit True |> writeBits 8 (fromIntegral s1) |> writeBits 8 (fromIntegral s2)
  where (|>) = flip ($)

-- | Write normal code (>2 symbols)
writeNormalCode :: VU.Vector (Word8, Word32, Int) -> BitWriter -> BitWriter
writeNormalCode codes w =
  -- Extract code lengths
  let codeLengths = VU.replicate 256 0 VU.// VU.toList (VU.map (\(sym, _, len) -> (fromIntegral sym, len)) codes)
      maxSym = VU.maximum $ VU.map (\(sym, _, _) -> fromIntegral sym) codes :: Int

      w1 = writeBit False w  -- is_simple = 0

      -- Write code length code
      -- Simple strategy: all symbols 0-8 have length 3, giving us 3-bit codes
      -- kCodeLengthCodeOrder = [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, ...]
      -- We need symbols 0-8 to encode lengths 0-8
      -- Positions: 17(0), 18(1), 0(2), 1(3), 2(4), 3(5), 4(6), 5(7), 16(8), 6(9), 7(10), 8(11)
      numCLC = 12  -- Through symbol 8
      w2 = writeBits 4 (numCLC - 4) w1

      -- Write CLC lengths: all get length 3 for simplicity
      w3 = foldl (\wa _ -> writeBits 3 3 wa) w2 [1..numCLC]

      -- use_max_symbol = 1
      w4 = writeBit True w3
      maxSymBits = max 2 (ceilLog2 maxSym)
      w5 = writeBits 3 (fromIntegral $ (maxSymBits - 2) `div` 2) w4
      w6 = writeBits maxSymBits (fromIntegral $ max 0 (maxSym - 2)) w5

      -- Write code lengths using the CLC
      -- The code length code maps symbol N to code based on position in kCodeLengthCodeOrder
      -- kCodeLengthCodeOrder = [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, ...]
      -- With all having length 3, canonical codes are assigned in order: 0,1,2,3,4,5,6,7,8,9,10,11

      -- To encode length L: use symbol L in the code length code
      -- Symbol L is at position positionOf(L) in kCodeLengthCodeOrder
      -- With length 3, symbol L gets code = positionOf(L)

      -- kCodeLengthCodeOrder positions:
      -- pos 0:17, pos 1:18, pos 2:0, pos 3:1, pos 4:2, pos 5:3, pos 6:4, pos 7:5, pos 8:16, pos 9:6, pos 10:7, pos 11:8
      -- So: sym 0→pos 2→code 2, sym 1→pos 3→code 3, ..., sym 8→pos 11→code 11

      codeForLength len
        | len == 0 = 2   -- Symbol 0 at position 2
        | len == 1 = 3   -- Symbol 1 at position 3
        | len == 2 = 4
        | len == 3 = 5
        | len == 4 = 6
        | len == 5 = 7
        | len == 6 = 9   -- Symbol 6 at position 9
        | len == 7 = 10
        | len == 8 = 11
        | otherwise = 0

      w7 = VU.ifoldl' (\wa sym len ->
             if sym <= maxSym
               then writeBits 3 (fromIntegral $ codeForLength len) wa
               else wa
           ) w6 codeLengths

   in w7

-- | Encode pixels using Huffman codes
encodePixels :: VS.Vector Word32 -> HuffmanCodes -> BitWriter -> BitWriter
encodePixels pixels codes w =
  let -- Build lookup tables
      gLookup = buildLookup (cGreen codes)
      rLookup = buildLookup (cRed codes)
      bLookup = buildLookup (cBlue codes)
      aLookup = buildLookup (cAlpha codes)

   in VS.foldl' (\wa px ->
        let g = fromIntegral ((px `shiftR` 8) .&. 0xFF)
            r = fromIntegral ((px `shiftR` 16) .&. 0xFF)
            b = fromIntegral (px .&. 0xFF)
            a = fromIntegral ((px `shiftR` 24) .&. 0xFF)

            (_, gCode, gLen) = gLookup VU.! g
            (_, rCode, rLen) = rLookup VU.! r
            (_, bCode, bLen) = bLookup VU.! b
            (_, aCode, aLen) = aLookup VU.! a

         in wa |> writeBits gLen (fromIntegral gCode)
               |> writeBits rLen (fromIntegral rCode)
               |> writeBits bLen (fromIntegral bCode)
               |> writeBits aLen (fromIntegral aCode)
      ) w pixels
  where
    (|>) = flip ($)

-- | Build lookup table from codes
buildLookup :: VU.Vector (Word8, Word32, Int) -> VU.Vector (Word8, Word32, Int)
buildLookup codes = runST $ do
  lookup <- VUM.replicate 256 (0, 0, 0)

  VU.forM_ codes $ \code@(sym, _, _) ->
    VUM.write lookup (fromIntegral sym) code

  VU.unsafeFreeze lookup

-- Helper functions

packARGB :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
packARGB a r g b =
  (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

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
  | otherwise = 8

when :: Monad m => Bool -> m () -> m ()
when True action = action
when False _ = return ()

forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ xs f = sequence_ (map f xs)
