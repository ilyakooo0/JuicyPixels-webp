{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8L.PrefixCode2
  ( PrefixCode (..),
    buildPrefixCode,
    decodeSymbol,
    readCodeLengths,
    kCodeLengthCodeOrder,
  )
where

import Codec.Picture.WebP.Internal.BitReader
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- | Prefix code (Huffman code) representation
data PrefixCode
  = PrefixCodeSingle !Word16
  | PrefixCodeTable !(VU.Vector Word32) !Int
  deriving (Show)

-- | Order in which code lengths are read for the code length code
kCodeLengthCodeOrder :: VU.Vector Int
kCodeLengthCodeOrder =
  VU.fromList [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

-- | Build a prefix code from code lengths - SIMPLIFIED VERSION
buildPrefixCode :: VU.Vector Int -> Either String PrefixCode
buildPrefixCode codeLengths
  | VU.null codeLengths = Left "Empty code lengths"
  | otherwise =
      let numSymbols = VU.length codeLengths
          nonZeroCount = VU.foldl' (\acc len -> if len > 0 then acc + 1 else acc) 0 codeLengths
       in case nonZeroCount of
            0 -> Left $ "No symbols with non-zero code length"
            1 ->
              let symbol = VU.head $ VU.filter (\i -> codeLengths VU.! i > 0) (VU.enumFromN 0 numSymbols)
               in Right $ PrefixCodeSingle (fromIntegral symbol)
            _ -> buildPrefixCodeTableSimple codeLengths

-- | Build lookup table using direct canonical code generation
buildPrefixCodeTableSimple :: VU.Vector Int -> Either String PrefixCode
buildPrefixCodeTableSimple codeLengths = runST $ do
  let numSymbols = VU.length codeLengths
      maxLen = VU.maximum codeLengths

  when (maxLen > 15) $
    error "Code length > 15 not supported"

  -- Count symbols at each length
  let histogram = VU.create $ do
        h <- VUM.replicate 16 (0 :: Int)
        VU.forM_ (VU.indexed codeLengths) $ \(_, len) ->
          when (len > 0) $ do
            c <- VUM.read h len
            VUM.write h len (c + 1)
        return h

  -- Compute next_code[] array (canonical code for each length)
  let nextCode = VU.create $ do
        nc <- VUM.replicate 16 (0 :: Int)
        codeVar <- VUM.new 1
        VUM.write codeVar 0 0
        forM_ [1 .. maxLen] $ \len -> do
          code <- VUM.read codeVar 0
          count <- return $ histogram VU.! len
          VUM.write nc len code
          VUM.write codeVar 0 ((code + count) `shiftL` 1)
        return nc

  -- Build table
  let tableSize = 1 `shiftL` 8  -- Primary table size
  table <- VUM.replicate tableSize (0xFFFFFFFF :: Word32)

  -- Track next code for each length (mutable copy)
  nextCodeMut <- VUM.new 16
  forM_ [0 .. 15] $ \i ->
    VUM.write nextCodeMut i (nextCode VU.! i)

  -- Assign codes and fill table
  VU.forM_ (VU.indexed codeLengths) $ \(symbol, len) ->
    when (len > 0 && len <= 8) $ do
      code <- VUM.read nextCodeMut len
      VUM.write nextCodeMut len (code + 1)  -- Increment for next symbol at this length

      let revCode = reverseBits code len
          entry = packEntry (fromIntegral symbol) len
          replicateCount = 1 `shiftL` (8 - len)

      -- Fill all table entries that match this prefix
      forM_ [0 .. replicateCount - 1] $ \i -> do
        let idx = revCode .|. (i `shiftL` len)
        VUM.write table idx entry

  frozenTable <- VU.unsafeFreeze table
  return $ Right $ PrefixCodeTable frozenTable 8

-- | Decode a single symbol from the bitstream
decodeSymbol :: PrefixCode -> BitReader -> (Word16, BitReader)
decodeSymbol (PrefixCodeSingle sym) reader = (sym, reader)
decodeSymbol (PrefixCodeTable table primaryBits) reader =
  let (peek, _) = readBits primaryBits reader
      entry = table VU.! fromIntegral peek
      symbol = fromIntegral (entry `shiftR` 16)
      len = fromIntegral (entry .&. 0xFFFF)
   in if entry == 0xFFFFFFFF
        then error $ "Invalid Huffman code for bit pattern " ++ show peek
        else
          let (_, reader') = readBits len reader
           in (symbol, reader')

-- | Read code lengths for a prefix code from the bitstream
readCodeLengths :: Int -> BitReader -> Either String (VU.Vector Int, BitReader)
readCodeLengths alphabetSize reader = do
  let (isSimple, reader1) = readBit reader

  if isSimple
    then do
      let (numSymbols1, reader2) = readBits 1 reader1
          numSymbols = fromIntegral numSymbols1 + 1
          (isFirst8Bits, reader2a) = readBit reader2
          symbolBits = if isFirst8Bits then 8 else 1
          (symbol1, reader3) = readBits symbolBits reader2a

      if numSymbols == 1
        then do
          let lengths = VU.replicate alphabetSize 0 VU.// [(fromIntegral symbol1, 1)]
          return (lengths, reader3)
        else do
          let (symbol2, reader4) = readBits 8 reader3
          let lengths =
                VU.replicate alphabetSize 0
                  VU.// [(fromIntegral symbol1, 1), (fromIntegral symbol2, 1)]
          return (lengths, reader4)
    else do
      let (numCodeLengths1, reader2) = readBits 4 reader1
          numCodeLengths = fromIntegral numCodeLengths1 + 4

      let (codeLengthLengths, reader3) = readCodeLengthLengths numCodeLengths reader2

      case buildPrefixCode codeLengthLengths of
        Left err -> Left $ "Failed to build code length code: " ++ err
        Right codeLengthCode -> do
          readSymbolCodeLengths alphabetSize codeLengthCode reader3

-- | Read code lengths for the code length alphabet
readCodeLengthLengths :: Int -> BitReader -> (VU.Vector Int, BitReader)
readCodeLengthLengths numCodeLengths reader = runST $ do
  lengths <- VUM.replicate 19 0
  let loop !i !r
        | i >= numCodeLengths = do
            frozen <- VU.unsafeFreeze lengths
            return (frozen, r)
        | otherwise = do
            let (len, r') = readBits 3 r
                idx = kCodeLengthCodeOrder VU.! i
            VUM.write lengths idx (fromIntegral len)
            loop (i + 1) r'
  loop 0 reader

-- | Read symbol code lengths using the code length code
readSymbolCodeLengths :: Int -> PrefixCode -> BitReader -> Either String (VU.Vector Int, BitReader)
readSymbolCodeLengths alphabetSize codeLengthCode reader = runST $ do
  let (useMaxSymbol, reader1) = readBit reader
      (maxSymbol, reader2) =
        if not useMaxSymbol
          then (alphabetSize, reader1)
          else
            let (lengthNbits1, reader1a) = readBits 3 reader1
                lengthNbits = 2 + 2 * fromIntegral lengthNbits1
                (maxSym1, reader1b) = readBits lengthNbits reader1a
                maxSym = 2 + fromIntegral maxSym1
             in (min maxSym alphabetSize, reader1b)

  lengths <- VUM.replicate alphabetSize 0
  prevCodeLen <- VUM.new 1
  VUM.write prevCodeLen 0 8

  let loop !i !r
        | i >= maxSymbol = do
            frozen <- VU.unsafeFreeze lengths
            return $ Right (frozen, r)
        | otherwise = do
            let (sym, r') = decodeSymbol codeLengthCode r
            case sym of
              _ | sym < 16 -> do
                VUM.write lengths i (fromIntegral sym)
                when (sym /= 0) $
                  VUM.write prevCodeLen 0 (fromIntegral sym)
                loop (i + 1) r'
              16 -> do
                let (extra, r'') = readBits 2 r'
                    repeatCount = 3 + fromIntegral extra
                prev <- VUM.read prevCodeLen 0
                forM_ [i .. min (i + repeatCount - 1) (maxSymbol - 1)] $ \j ->
                  VUM.write lengths j prev
                loop (i + repeatCount) r''
              17 -> do
                let (extra, r'') = readBits 3 r'
                    repeatCount = 3 + fromIntegral extra
                loop (i + repeatCount) r''
              18 -> do
                let (extra, r'') = readBits 7 r'
                    repeatCount = 11 + fromIntegral extra
                loop (i + repeatCount) r''
              _ -> return $ Left $ "Invalid code length symbol: " ++ show sym

  loop 0 reader2

-- Helper functions
packEntry :: Word16 -> Int -> Word32
packEntry symbol len = (fromIntegral symbol `shiftL` 16) .|. fromIntegral len

reverseBits :: Int -> Int -> Int
reverseBits value numBits = go 0 value numBits
  where
    go !acc !val !n
      | n <= 0 = acc
      | otherwise =
          let acc' = (acc `shiftL` 1) .|. (val .&. 1)
              val' = val `shiftR` 1
           in go acc' val' (n - 1)
