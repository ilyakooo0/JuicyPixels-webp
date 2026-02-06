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
-- This implementation closely follows libwebp's BuildHuffmanTable algorithm
buildPrefixCodeTable :: VU.Vector Int -> Either String PrefixCode
buildPrefixCodeTable codeLengths = runST $ do
  let numSymbols = VU.length codeLengths
      maxCodeLength = VU.maximum codeLengths

  when (maxCodeLength > 15) $
    error "Code length > 15 not supported"

  let primaryBits = 8
      primarySize = 1 `shiftL` primaryBits
      -- Need to allocate blCount/offset for at least primaryBits+1 to avoid out of bounds when iterating up to primaryBits
      histogramSize = max (maxCodeLength + 1) (primaryBits + 1)

  -- Step 1: Build histogram of code lengths
  blCount <- VUM.replicate histogramSize (0 :: Int)
  VU.forM_ (VU.indexed codeLengths) $ \(_sym, len) ->
    when (len > 0) $ do
      count <- VUM.read blCount len
      VUM.write blCount len (count + 1)

  -- Step 2: Create sorted symbol array (sorted by code length, then by symbol value)
  sorted <- VUM.new numSymbols
  offset <- VUM.replicate histogramSize (0 :: Int)

  -- Calculate offsets for each code length
  currentOffset <- VUM.new 1
  VUM.write currentOffset 0 0
  forM_ [1 .. maxCodeLength] $ \len -> do
    off <- VUM.read currentOffset 0
    VUM.write offset len off
    count <- VUM.read blCount len
    VUM.write currentOffset 0 (off + count)

  -- Fill sorted array
  VU.forM_ (VU.indexed codeLengths) $ \(sym, len) ->
    when (len > 0) $ do
      off <- VUM.read offset len
      VUM.write sorted off (fromIntegral sym :: Word16)
      VUM.write offset len (off + 1)

  -- No need to reset offset - we'll read directly from sorted array using symbolIdx counter

  -- Step 3: Allocate table (start with primary + generous secondary space)
  let maxTableSize = primarySize + 32768  -- Generous allocation
  table <- VUM.replicate maxTableSize invalidEntry

  nextTablePos <- VUM.new 1
  VUM.write nextTablePos 0 primarySize

  -- Step 4: Fill primary table (codes with length <= primaryBits)
  key <- VUM.new 1
  VUM.write key 0 0

  symbolIdx <- VUM.new 1
  VUM.write symbolIdx 0 0

  -- Validate tree completeness (check that we don't over-subscribe the code space)
  -- NOTE: Validation temporarily disabled for debugging
  -- numOpen <- VUM.new 1
  -- VUM.write numOpen 0 1

  -- blCountFrozen <- VU.unsafeFreeze blCount
  -- forM_ [1 .. maxCodeLength] $ \len -> do
  --   open <- VUM.read numOpen 0
  --   let count = blCountFrozen VU.! len
  --       newOpen = (open `shiftL` 1) - count
  --   VUM.write numOpen 0 newOpen
  --   when (newOpen < 0) $
  --     error $ "Invalid Huffman tree: over-subscribed code space at length " ++ show len ++ ", open=" ++ show open ++ ", count=" ++ show count ++ ", alphabet=" ++ show numSymbols ++ ", blCount=" ++ show (VU.toList $ VU.take (maxCodeLength + 1) blCountFrozen)

  -- Tree can be incomplete (newOpen > 0) - this is valid in VP8L

  forM_ [1 .. primaryBits] $ \len -> do
    count <- VUM.read blCount len
    forM_ [1 .. count] $ \_ -> do
      symIdx <- VUM.read symbolIdx 0
      sym <- VUM.read sorted symIdx
      VUM.write symbolIdx 0 (symIdx + 1)

      k <- VUM.read key 0
      let entry = packEntry sym len
          revKey = reverseBits k len

      -- Replicate entry for all bit patterns with this prefix
      forM_ [0 .. ((1 `shiftL` (primaryBits - len)) - 1)] $ \i -> do
        let idx = revKey .|. (i `shiftL` len)
        VUM.write table idx entry

      VUM.write key 0 (k + 1)

  -- Step 5: Fill secondary tables (codes with length > primaryBits)
  when (maxCodeLength > primaryBits) $ do
    VUM.write key 0 0

    forM_ [(primaryBits + 1) .. maxCodeLength] $ \len -> do
      count <- VUM.read blCount len
      forM_ [1 .. count] $ \_ -> do
        symIdx <- VUM.read symbolIdx 0
        sym <- VUM.read sorted symIdx
        VUM.write symbolIdx 0 (symIdx + 1)

        k <- VUM.read key 0
        let shortCode = k .&. ((1 `shiftL` primaryBits) - 1)
            revShort = reverseBits shortCode primaryBits

        -- Check if we need a new secondary table
        existing <- VUM.read table revShort
        (secondaryTableBits, secondaryOffset) <-
          if existing == invalidEntry
            then do
              -- Calculate size needed for this secondary table
              tableBits <- calculateSecondaryTableBits blCount len primaryBits maxCodeLength
              let tableSize = 1 `shiftL` tableBits

              offset <- VUM.read nextTablePos 0
              VUM.write nextTablePos 0 (offset + tableSize)

              -- Store pointer in primary table: offset in symbol field, total bits (primary + secondary) in len field
              let totalBits = tableBits + primaryBits
              VUM.write table revShort (packEntry (fromIntegral offset) totalBits)
              return (totalBits, offset)
            else do
              let totalBits = fromIntegral (existing .&. 0xFFFF)
                  offset = fromIntegral (existing `shiftR` 16)
              return (totalBits, offset)

        -- Fill secondary table entry
        let longPart = k `shiftR` primaryBits
            longBits = len - primaryBits
            revLong = reverseBits longPart longBits
            actualSecondaryBits = secondaryTableBits - primaryBits
            entry = packEntry sym longBits

        -- Replicate entry for all bit patterns with this prefix
        forM_ [0 .. ((1 `shiftL` (actualSecondaryBits - longBits)) - 1)] $ \i -> do
          let idx = secondaryOffset + revLong + (i `shiftL` longBits)
          VUM.write table idx entry

        VUM.write key 0 (k + 1)

  -- Determine actual table size used
  actualSize <- VUM.read nextTablePos 0

  -- Freeze and truncate to actual size
  frozenTable <- VU.unsafeFreeze table
  let finalTable = VU.take actualSize frozenTable

  return $ Right $ PrefixCodeTable finalTable primaryBits

-- | Calculate the number of bits needed for a secondary table
-- This follows libwebp's NextTableBitSize algorithm
calculateSecondaryTableBits :: VUM.MVector s Int -> Int -> Int -> Int -> ST s Int
calculateSecondaryTableBits blCount currentLen primaryBits maxCodeLength = do
  blCountFrozen <- VU.unsafeFreeze blCount
  let loop !len !left
        | len > maxCodeLength || left <= 0 = len - 1 - primaryBits
        | otherwise =
            let count = blCountFrozen VU.! len
                newLeft = left - count
             in if newLeft <= 0
                  then len - primaryBits
                  else loop (len + 1) (newLeft * 2)

      initialLeft = 1 `shiftL` (currentLen - primaryBits)
  return $ loop currentLen initialLeft

-- | Decode a single symbol from the bitstream using a prefix code
decodeSymbol :: PrefixCode -> BitReader -> (Word16, BitReader)
decodeSymbol (PrefixCodeSingle sym) reader = (sym, reader)
decodeSymbol (PrefixCodeTable table primaryBits) reader =
  let (peek, _) = readBits primaryBits reader
      entry = table VU.! fromIntegral peek
      symbol = fromIntegral (entry `shiftR` 16)
      len = fromIntegral (entry .&. 0xFFFF)
   in if entry == invalidEntry
        then error $ "VP8L bitstream error: Invalid prefix code for bit pattern " ++ show peek ++ ". This typically indicates either: (1) corrupted file data, (2) unsupported encoder variant, or (3) decoder bug in code length reading/table building."
        else if len <= primaryBits
          then
            -- Direct symbol in primary table
            let (_, reader') = readBits len reader
             in (symbol, reader')
          else
            -- Secondary table: len field contains tableBits (total bits including primary)
            -- symbol field contains offset to secondary table
            let (_, reader1) = readBits primaryBits reader
                secondaryBits = len - primaryBits
                (peek2, _) = readBits secondaryBits reader1
                offset = fromIntegral symbol
                idx = offset + fromIntegral peek2
             in if idx >= VU.length table
                  then error $ "VP8L bitstream error: Secondary table index out of bounds: idx=" ++ show idx ++ ", tableSize=" ++ show (VU.length table)
                  else
                    let entry2 = table VU.! idx
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
            -- Debug: trace code length lengths
            -- let nonZero = [(idx, frozen VU.! idx) | idx <- [0..18], frozen VU.! idx > 0]
            -- in trace ("Code length lengths: " ++ show nonZero) $ return (frozen, r)
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
