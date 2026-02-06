{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8L.PrefixCode
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
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- | Prefix code (Huffman code) representation
-- Either a single symbol (0-bit code) or a lookup table
data PrefixCode
  = PrefixCodeSingle !Word16
  | PrefixCodeTable !(VU.Vector Word32) !Int
  deriving (Show)

-- | Order in which code lengths are read for the code length code
kCodeLengthCodeOrder :: VU.Vector Int
kCodeLengthCodeOrder =
  VU.fromList [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

-- | Build a prefix code from code lengths
-- Returns Left error message on invalid input
buildPrefixCode :: VU.Vector Int -> Either String PrefixCode
buildPrefixCode codeLengths
  | VU.null codeLengths = Left "Empty code lengths"
  | otherwise =
      let numSymbols = VU.length codeLengths
          nonZeroCount = VU.foldl' (\acc len -> if len > 0 then acc + 1 else acc) 0 codeLengths
       in case nonZeroCount of
            0 -> Left $ "No symbols with non-zero code length (alphabet size: " ++ show numSymbols ++ ")"
            1 ->
              let symbol = VU.head $ VU.filter (\i -> codeLengths VU.! i > 0) (VU.enumFromN 0 numSymbols)
               in Right $ PrefixCodeSingle (fromIntegral symbol)
            _ -> buildPrefixCodeTable codeLengths

-- | Build a two-level lookup table for prefix code decoding
buildPrefixCodeTable :: VU.Vector Int -> Either String PrefixCode
buildPrefixCodeTable codeLengths = runST $ do
  let numSymbols = VU.length codeLengths
      maxCodeLength = VU.maximum codeLengths

  when (maxCodeLength > 15) $
    error "Code length > 15 not supported"

  let primaryBits = 8
      primarySize = 1 `shiftL` primaryBits

  blCount <- VUM.replicate (maxCodeLength + 1) (0 :: Int)
  VU.forM_ (VU.indexed codeLengths) $ \(sym, len) ->
    when (len > 0) $ do
      count <- VUM.read blCount len
      VUM.write blCount len (count + 1)

  nextCode <- VUM.replicate (maxCodeLength + 1) (0 :: Int)
  code <- VUM.new 1
  VUM.write code 0 0
  forM_ [1 .. maxCodeLength] $ \bits -> do
    codeVal <- VUM.read code 0
    count <- VUM.read blCount bits
    let newCode = (codeVal + count) `shiftL` 1
    VUM.write code 0 newCode
    VUM.write nextCode bits codeVal

  table <- VUM.replicate (primarySize + 2048) invalidEntry

  -- Counter for next available secondary table offset
  nextSecondaryOffset <- VUM.new 1
  VUM.write nextSecondaryOffset 0 primarySize

  VU.forM_ (VU.indexed codeLengths) $ \(symbol, len) ->
    when (len > 0) $ do
      codeVal <- VUM.read nextCode len
      VUM.write nextCode len (codeVal + 1)

      let sym16 = fromIntegral symbol :: Word16
          entry = packEntry sym16 len

      if len <= primaryBits
        then do
          let replicateCount = 1 `shiftL` (primaryBits - len)
              reversedCode = reverseBits codeVal len
          forM_ [0 .. replicateCount - 1] $ \i -> do
            let idx = reversedCode .|. (i `shiftL` len)
            VUM.write table idx entry
        else do
          let shortCode = codeVal .&. ((1 `shiftL` primaryBits) - 1)
              reversedShort = reverseBits shortCode primaryBits
          existing <- VUM.read table reversedShort
          secondaryOffset <-
            if existing == invalidEntry
              then do
                offset <- VUM.read nextSecondaryOffset 0
                VUM.write nextSecondaryOffset 0 (offset + 64)
                VUM.write table reversedShort (packEntry 0 (offset `shiftR` 16))
                return offset
              else do
                let offset = (fromIntegral existing .&. 0xFFFF) `shiftL` 16
                return offset

          let longPart = codeVal `shiftR` primaryBits
              longBits = len - primaryBits
              reversedLong = reverseBits longPart longBits
              replicateCount = 1 `shiftL` (15 - len)
          forM_ [0 .. replicateCount - 1] $ \i -> do
            let idx = secondaryOffset + reversedLong + (i `shiftL` longBits)
            VUM.write table idx entry

  frozenTable <- VU.unsafeFreeze table
  return $ Right $ PrefixCodeTable frozenTable primaryBits

-- | Decode a single symbol from the bitstream using a prefix code
decodeSymbol :: PrefixCode -> BitReader -> (Word16, BitReader)
decodeSymbol (PrefixCodeSingle sym) reader = (sym, reader)
decodeSymbol (PrefixCodeTable table primaryBits) reader =
  let (peek, _) = readBits primaryBits reader
      entry = table VU.! fromIntegral peek
      symbol = fromIntegral (entry `shiftR` 16)
      len = fromIntegral (entry .&. 0xFFFF)
   in if len <= primaryBits
        then
          let (_, reader') = readBits len reader
           in (symbol, reader')
        else
          let (_, reader1) = readBits primaryBits reader
              (peek2, _) = readBits 7 reader1
              offset = len `shiftL` 16
              idx = offset + fromIntegral peek2
              entry2 = table VU.! idx
              symbol2 = fromIntegral (entry2 `shiftR` 16)
              len2 = fromIntegral (entry2 .&. 0xFFFF)
              (_, reader2) = readBits len2 reader1
           in (symbol2, reader2)

-- | Read code lengths for a prefix code from the bitstream
-- Handles simple codes (1-2 symbols) and normal codes with repeat codes
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
  -- Read max_symbol
  let (useMaxSymbol, reader1) = readBit reader
      (maxSymbol, reader2) =
        if not useMaxSymbol
          then (alphabetSize, reader1)
          else
            let (lengthNbits1, reader1a) = readBits 3 reader1
                lengthNbits = 2 + 2 * fromIntegral lengthNbits1
                (maxSym1, reader1b) = readBits lengthNbits reader1a
                maxSym = 2 + fromIntegral maxSym1
             in (min maxSym alphabetSize, reader1b)  -- Clamp to alphabet_size

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
                let writeRepeats !j !r2
                      | j >= i + repeatCount = loop (i + repeatCount) r2
                      | j >= maxSymbol = loop maxSymbol r2
                      | otherwise = do
                          VUM.write lengths j prev
                          writeRepeats (j + 1) r2
                writeRepeats i r''
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

invalidEntry :: Word32
invalidEntry = 0xFFFFFFFF

reverseBits :: Int -> Int -> Int
reverseBits value numBits = go 0 value numBits
  where
    go !acc !val !n
      | n <= 0 = acc
      | otherwise =
          let acc' = (acc `shiftL` 1) .|. (val .&. 1)
              val' = val `shiftR` 1
           in go acc' val' (n - 1)
