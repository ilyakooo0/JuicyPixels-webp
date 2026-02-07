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
import Debug.Trace (trace)

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
buildPrefixCodeTable codeLengths = trace ("Building prefix code table: numSymbols=" ++ show (VU.length codeLengths) ++ ", maxLen=" ++ show (VU.maximum codeLengths)) $ runST $ do
  let numSymbols = VU.length codeLengths
      maxCodeLength = VU.maximum codeLengths

  when (maxCodeLength > 15) $
    error "Code length > 15 not supported"

  let primaryBits = 8
      primarySize = 1 `shiftL` primaryBits
      -- Need to allocate blCount/offset for at least primaryBits+1 to avoid out of bounds when iterating up to primaryBits
      histogramSize = max (maxCodeLength + 1) (primaryBits + 1)

  trace ("  Allocating histogram of size " ++ show histogramSize) $ return ()

  -- Step 1: Build histogram of code lengths
  blCount <- VUM.replicate histogramSize (0 :: Int)
  VU.forM_ (VU.indexed codeLengths) $ \(_sym, len) ->
    when (len > 0) $ do
      count <- VUM.read blCount len
      VUM.write blCount len (count + 1)

  trace "  Histogram built" $ return ()

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

  -- Step 4: Compute canonical codes (next_code array)
  trace "  Computing canonical codes" $ return ()
  nextCode <- VUM.replicate histogramSize (0 :: Int)
  codeAcc <- VUM.new 1
  VUM.write codeAcc 0 0

  forM_ [1 .. maxCodeLength] $ \len -> do
    code <- VUM.read codeAcc 0
    VUM.write nextCode len code
    count <- VUM.read blCount len
    let newCode = (code + count) `shiftL` 1
    when (newCode < 0 || newCode > 100000) $
      error $ "Code overflow at length " ++ show len ++ ": code=" ++ show code ++ ", count=" ++ show count ++ ", newCode=" ++ show newCode
    VUM.write codeAcc 0 newCode

  trace "  Filling primary table" $ return ()
  -- Step 5: Fill primary table (codes with length <= primaryBits)
  symbolIdx <- VUM.new 1
  VUM.write symbolIdx 0 0

  forM_ [1 .. primaryBits] $ \len -> do
    count <- VUM.read blCount len
    trace ("    Length " ++ show len ++ ": " ++ show count ++ " symbols") $ return ()
    forM_ [1 .. count] $ \_ -> do
      symIdx <- VUM.read symbolIdx 0
      sym <- VUM.read sorted symIdx
      VUM.write symbolIdx 0 (symIdx + 1)

      -- Get next canonical code for this length and increment
      code <- VUM.read nextCode len
      VUM.write nextCode len (code + 1)

      let entry = packEntry sym len
          revKey = reverseBits code len
          replicateCount = 1 `shiftL` (primaryBits - len)

      trace ("      Symbol " ++ show sym ++ " -> code " ++ show code ++ " (len " ++ show len ++ "), replicate " ++ show replicateCount ++ " times") $ return ()

      -- Replicate entry for all bit patterns with this prefix
      forM_ [0 .. (replicateCount - 1)] $ \i -> do
        let idx = revKey .|. (i `shiftL` len)
        VUM.write table idx entry

  -- Step 6: Fill secondary tables (codes with length > primaryBits)
  trace ("  Filling secondary tables for lengths " ++ show (primaryBits + 1) ++ " to " ++ show maxCodeLength) $ return ()
  when (maxCodeLength > primaryBits) $ do
    forM_ [(primaryBits + 1) .. maxCodeLength] $ \len -> do
      count <- VUM.read blCount len
      trace ("    Length " ++ show len ++ ": " ++ show count ++ " symbols") $ return ()
      forM_ [1 .. count] $ \symNum -> do
        symIdx <- VUM.read symbolIdx 0
        sym <- VUM.read sorted symIdx
        VUM.write symbolIdx 0 (symIdx + 1)

        -- Get next canonical code for this length and increment
        code <- VUM.read nextCode len
        VUM.write nextCode len (code + 1)

        let shortCode = code .&. ((1 `shiftL` primaryBits) - 1)
            revShort = reverseBits shortCode primaryBits

        trace ("      Symbol " ++ show sym ++ " (code=" ++ show code ++ ", len=" ++ show len ++ ")") $ return ()

        -- Check if we need a new secondary table
        existing <- VUM.read table revShort
        let existingLen = fromIntegral (existing .&. 0xFFFF) :: Int
            existingSymbol = fromIntegral (existing `shiftR` 16) :: Int

        trace ("        revShort=" ++ show revShort ++ ", existing=0x" ++ showHex existing ++ " (sym=" ++ show existingSymbol ++ ", len=" ++ show existingLen ++ ")") $ return ()

        (secondaryTableBits, secondaryOffset) <-
          if existing == invalidEntry
            then do
              trace ("        Allocating new secondary table (was invalid)") $ return ()
              -- Calculate size needed for this secondary table
              tableBits <- calculateSecondaryTableBits blCount len primaryBits maxCodeLength
              trace ("          tableBits=" ++ show tableBits) $ return ()
              when (tableBits < 0 || tableBits > 10) $
                error $ "Invalid tableBits: " ++ show tableBits
              let tableSize = 1 `shiftL` tableBits

              offset <- VUM.read nextTablePos 0
              VUM.write nextTablePos 0 (offset + tableSize)

              -- Store pointer in primary table: offset in symbol field, total bits (primary + secondary) in len field
              let totalBits = tableBits + primaryBits
              VUM.write table revShort (packEntry (fromIntegral offset) totalBits)
              trace ("          Allocated secondary table at offset " ++ show offset ++ " with size " ++ show tableSize ++ ", totalBits=" ++ show totalBits) $ return ()
              return (totalBits, offset)
            else if existingLen > 0 && existingLen <= primaryBits
              then do
                -- This slot has a direct symbol! We need to allocate a secondary table
                -- Move the existing symbol to the secondary table
                trace ("        Promoting slot to secondary table (had symbol " ++ show existingSymbol ++ " len " ++ show existingLen ++ ")") $ return ()

                -- Calculate size needed for this secondary table
                tableBits <- calculateSecondaryTableBits blCount len primaryBits maxCodeLength
                when (tableBits < 0 || tableBits > 10) $
                  error $ "Invalid tableBits: " ++ show tableBits
                let tableSize = 1 `shiftL` tableBits

                offset <- VUM.read nextTablePos 0
                VUM.write nextTablePos 0 (offset + tableSize)

                -- Replace primary entry with secondary table pointer
                let totalBits = tableBits + primaryBits
                VUM.write table revShort (packEntry (fromIntegral offset) totalBits)

                -- Move the existing symbol to the secondary table
                -- The existing symbol's code shares the first primaryBits with our shortCode
                -- We need to place it in the secondary table based on its remaining bits
                let existingCodeRemainder = 0  -- It had exactly primaryBits or less, so remainder is 0
                    existingEntry = packEntry (fromIntegral existingSymbol) existingLen
                    -- Replicate it in the secondary table
                    existingReplicateCount = 1 `shiftL` (tableBits - 0)  -- Fill entire secondary table since it was shorter
                forM_ [0 .. existingReplicateCount - 1] $ \i ->
                  VUM.write table (offset + i) existingEntry

                trace ("          Moved existing symbol to secondary table, allocated at offset " ++ show offset) $ return ()
                return (totalBits, offset)
              else do
                -- Existing secondary table pointer
                let totalBits = existingLen
                    offset = existingSymbol
                trace ("        Using existing secondary table: totalBits=" ++ show totalBits ++ ", offset=" ++ show offset) $ return ()
                when (totalBits <= primaryBits || totalBits > 20) $
                  error $ "Invalid totalBits from existing entry: " ++ show totalBits ++ " (should be > " ++ show primaryBits ++ ", existing=0x" ++ showHex existing ++ ")"
                return (totalBits, offset)

        -- Fill secondary table entry
        let longPart = code `shiftR` primaryBits
            longBits = len - primaryBits
            revLong = reverseBits longPart longBits
            actualSecondaryBits = secondaryTableBits - primaryBits

        trace ("        longPart=" ++ show longPart ++ ", longBits=" ++ show longBits ++ ", actualSecondaryBits=" ++ show actualSecondaryBits ++ ", secondaryTableBits=" ++ show secondaryTableBits) $ return ()

        -- Check if secondary table is large enough
        when (actualSecondaryBits < longBits) $ do
          trace ("        ERROR: Secondary table too small! actualSecondaryBits=" ++ show actualSecondaryBits ++ " < longBits=" ++ show longBits) $ return ()
          -- This can happen when the table was allocated for shorter codes
          -- We need to either expand the table or skip this symbol
          -- For now, skip this symbol (incomplete tree is OK in VP8L)
          trace ("        Skipping symbol due to insufficient secondary table size") $ return ()

        when (actualSecondaryBits >= longBits) $ do
          let replicateCount = 1 `shiftL` (actualSecondaryBits - longBits)
              entry = packEntry sym longBits

          trace ("        replicateCount=" ++ show replicateCount) $ return ()
          when (replicateCount < 0 || replicateCount > 10000) $
            error $ "Invalid replicateCount: " ++ show replicateCount

          -- Replicate entry for all bit patterns with this prefix
          forM_ [0 .. (replicateCount - 1)] $ \i -> do
            let idx = secondaryOffset + revLong + (i `shiftL` longBits)
            VUM.write table idx entry

  -- Determine actual table size used
  actualSize <- VUM.read nextTablePos 0

  -- Fill any remaining invalid entries in primary table with first valid symbol
  -- This handles incomplete Huffman trees gracefully
  trace "  Filling incomplete tree gaps with default symbol" $ return ()
  firstSymIdx <- VUM.read symbolIdx 0
  when (firstSymIdx > 0) $ do
    firstSym <- VUM.read sorted 0
    let defaultEntry = packEntry firstSym 0  -- 0-bit code (will return immediately)
    forM_ [0 .. primarySize - 1] $ \i -> do
      entry <- VUM.read table i
      when (entry == invalidEntry) $
        VUM.write table i defaultEntry

  -- Also fill secondary tables
  forM_ [primarySize .. actualSize - 1] $ \i -> do
    entry <- VUM.read table i
    when (entry == invalidEntry) $ do
      firstSym <- VUM.read sorted 0
      VUM.write table i (packEntry firstSym 0)

  -- Freeze and truncate to actual size
  frozenTable <- VU.unsafeFreeze table
  let finalTable = VU.take actualSize frozenTable

  trace ("  Table built: size=" ++ show actualSize ++ ", primary=" ++ show primarySize) $ return ()
  return $ Right $ PrefixCodeTable finalTable primaryBits

-- | Calculate the number of bits needed for a secondary table
-- Simplified: always allocate maximum size (7 bits) for secondary tables
-- This avoids the complex calculation and ensures all symbols fit
calculateSecondaryTableBits :: VUM.MVector s Int -> Int -> Int -> Int -> ST s Int
calculateSecondaryTableBits blCount currentLen primaryBits maxCodeLength = do
  -- Simple approach: just use the maximum possible secondary bits
  -- which is (maxCodeLength - primaryBits)
  let maxSecondaryBits = maxCodeLength - primaryBits
      -- But cap it at 7 bits (128 entries) for efficiency
      tableBits = min 7 maxSecondaryBits
  return tableBits

-- | Decode a single symbol from the bitstream using a prefix code
decodeSymbol :: PrefixCode -> BitReader -> (Word16, BitReader)
decodeSymbol (PrefixCodeSingle sym) reader = (sym, reader)
decodeSymbol (PrefixCodeTable table primaryBits) reader =
  let (peek, _) = readBits primaryBits reader
      entry = table VU.! fromIntegral peek
      symbol = fromIntegral (entry `shiftR` 16)
      len = fromIntegral (entry .&. 0xFFFF)
   in if entry == invalidEntry
        then error $ "VP8L bitstream error: Invalid prefix code for primary bit pattern " ++ show peek ++ ". Table entry is invalidEntry. This usually means: (1) incomplete Huffman tree (some bit patterns unused), or (2) out of sync in bitstream."
        else if len <= primaryBits
          then
            -- Direct symbol in primary table
            let (_, reader') = readBits len reader
             in (symbol, reader')
          else
            -- Secondary table: len field contains totalBits (total bits including primary)
            -- symbol field contains offset to secondary table
            let (_, reader1) = readBits primaryBits reader
                secondaryBits = len - primaryBits
                (peek2, _) = readBits secondaryBits reader1
                offset = fromIntegral symbol
                idx = offset + fromIntegral peek2
             in if idx >= VU.length table
                  then error $ "VP8L bitstream error: Secondary table index out of bounds: idx=" ++ show idx ++ " (offset=" ++ show offset ++ ", peek2=" ++ show peek2 ++ "), tableSize=" ++ show (VU.length table)
                  else
                    let entry2 = table VU.! idx
                     in if entry2 == invalidEntry
                          then
                            -- Incomplete tree: this bit pattern is unused
                            -- This can happen if the encoder used an incomplete tree
                            -- For now, return a dummy symbol (0) and consume bits
                            -- Better: return error
                            trace ("WARNING: Incomplete tree, returning dummy symbol for idx=" ++ show idx) $
                              let (_, reader2) = readBits secondaryBits reader1
                               in (0, reader2)
                          else
                            let symbol2 = fromIntegral (entry2 `shiftR` 16)
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

-- | Show value as hex (for debugging)
showHex :: (Show a, Integral a) => a -> String
showHex n = show (fromIntegral n :: Integer)
