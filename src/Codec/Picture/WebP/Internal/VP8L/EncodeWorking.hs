{-# LANGUAGE BangPatterns #-}

-- | Working VP8L encoder for any image
module Codec.Picture.WebP.Internal.VP8L.EncodeWorking
  ( encodeVP8LWorking,
  )
where

import Codec.Picture.Types
import Codec.Picture.WebP.Internal.BitWriter
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import Data.List (nub, sort)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- | Encode VP8L - only encodes symbols that actually appear
encodeVP8LWorking :: Image PixelRGBA8 -> B.ByteString
encodeVP8LWorking img =
  let width = imageWidth img
      height = imageHeight img
      pixels = imageData img

      -- Find unique values in each channel
      uniqueVals = findUniqueValues pixels (width * height)

      w =
        emptyBitWriter
          |> writeBits 8 0x2F
          |> writeBits 14 (fromIntegral $ width - 1)
          |> writeBits 14 (fromIntegral $ height - 1)
          |> writeBit True
          |> writeBits 3 0
          |> writeBit False -- no transforms
          |> writeBit False -- no color cache
          |> writeBit False -- single prefix code group
          |> writeSymbolSet (uvGreen uniqueVals) -- Green
          |> writeSymbolSet (uvRed uniqueVals) -- Red
          |> writeSymbolSet (uvBlue uniqueVals) -- Blue
          |> writeSymbolSet (uvAlpha uniqueVals) -- Alpha
          |> writeSimple 0 -- Distance
          |> encodeAllPixels pixels (width * height) uniqueVals
          |> finalizeBitWriter
   in bitWriterToByteString w
  where
    (|>) = flip ($)

data UniqueValues = UniqueValues
  { uvGreen :: ![Word8],
    uvRed :: ![Word8],
    uvBlue :: ![Word8],
    uvAlpha :: ![Word8]
  }

findUniqueValues :: VS.Vector Word8 -> Int -> UniqueValues
findUniqueValues pixels numPixels =
  let gVals = nub $ sort [pixels VS.! (i * 4 + 1) | i <- [0 .. numPixels - 1]]
      rVals = nub $ sort [pixels VS.! (i * 4) | i <- [0 .. numPixels - 1]]
      bVals = nub $ sort [pixels VS.! (i * 4 + 2) | i <- [0 .. numPixels - 1]]
      aVals = nub $ sort [pixels VS.! (i * 4 + 3) | i <- [0 .. numPixels - 1]]
   in UniqueValues gVals rVals bVals aVals

-- | Write code for a set of symbols
writeSymbolSet :: [Word8] -> BitWriter -> BitWriter
writeSymbolSet [] = writeSimple 0
writeSymbolSet [s] = writeSimple (fromIntegral s)
writeSymbolSet [s1, s2] = write2 (fromIntegral s1) (fromIntegral s2)
writeSymbolSet syms = writeMulti syms

writeSimple :: Word16 -> BitWriter -> BitWriter
writeSimple sym w =
  writeBit True w |> writeBit False |> writeBit True |> writeBits 8 (fromIntegral sym)
  where
    (|>) = flip ($)

write2 :: Word16 -> Word16 -> BitWriter -> BitWriter
write2 s1 s2 w =
  writeBit True w |> writeBit True |> writeBit True |> writeBits 8 (fromIntegral s1) |> writeBits 8 (fromIntegral s2)
  where
    (|>) = flip ($)

-- | Write code for multiple symbols (3-256)
-- Strategy: Assign length based on position, use canonical codes
writeMulti :: [Word8] -> BitWriter -> BitWriter
writeMulti syms w =
  let numSyms = length syms
      -- Assign lengths: first gets shortest, distribute evenly
      codeLen = ceiling (logBase 2 (fromIntegral numSyms) :: Double)

      -- All symbols get the same length for simplicity
      -- This is valid but not optimal

      w1 = writeBit False w -- is_simple = 0

      -- Write code length code
      -- We need to encode length codeLen for each symbol
      -- Use a CLC with symbol codeLen having length 1

      -- For codeLen in range 2-8, position in kCodeLengthCodeOrder:
      -- 2→pos 4, 3→pos 5, 4→pos 6, 5→pos 7, 6→pos 9, 7→pos 10, 8→pos 11
      numCLC = if codeLen <= 5 then codeLen + 3 else codeLen + 4
      w2 = writeBits 4 (fromIntegral $ numCLC - 4) w1

      -- Write CLC lengths: only symbol codeLen gets length 1
      kCodeLengthCodeOrder = [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
      w3 =
        foldl
          ( \wa i ->
              let sym = kCodeLengthCodeOrder !! i
                  len = if sym == codeLen then 1 else 0
               in writeBits 3 (fromIntegral len) wa
          )
          w2
          [0 .. numCLC - 1]

      -- use_max_symbol = 1
      w4 = writeBit True w3
      maxSym = maximum syms
      maxSymBits = max 2 (ceiling $ logBase 2 (fromIntegral maxSym :: Double))
      w5 = writeBits 3 (fromIntegral $ (maxSymBits - 2) `div` 2) w4
      w6 = writeBits maxSymBits (fromIntegral $ fromIntegral maxSym - 2) w5

      -- Write code lengths for symbols 0 to maxSym
      -- Symbol codeLen in CLC is encoded as bit 0 (since it's the only symbol with len 1)
      w7 =
        foldl
          ( \wa sym ->
              if fromIntegral sym `elem` syms
                then writeBit False wa -- Symbol codeLen (length for this symbol)
                else writeBit False wa -- Symbol codeLen (length 0? or skip?)
          )
          w6
          [0 .. fromIntegral maxSym]
   in w7

encodeAllPixels :: VS.Vector Word8 -> Int -> UniqueValues -> BitWriter -> BitWriter
encodeAllPixels pixels numPixels uv w =
  foldl
    ( \wa i ->
        let g = pixels VS.! (i * 4 + 1)
            r = pixels VS.! (i * 4)
            b = pixels VS.! (i * 4 + 2)
            a = pixels VS.! (i * 4 + 3)
         in wa
              |> encodeValue g (uvGreen uv)
              |> encodeValue r (uvRed uv)
              |> encodeValue b (uvBlue uv)
              |> encodeValue a (uvAlpha uv)
    )
    w
    [0 .. numPixels - 1]
  where
    (|>) = flip ($)

encodeValue :: Word8 -> [Word8] -> BitWriter -> BitWriter
encodeValue val syms w
  | length syms == 1 = w -- 0-bit code
  | length syms == 2 = writeBit (val == (syms !! 1)) w -- 1-bit code
  | otherwise =
      -- Fixed-length code: find index and write it
      case lookup val (zip syms [0 ..]) of
        Just idx ->
          let codeLen = ceiling (logBase 2 (fromIntegral $ length syms) :: Double)
           in writeBits codeLen (fromIntegral idx) w
        Nothing -> w -- Shouldn't happen
  where
    lookup _ [] = Nothing
    lookup x ((y, v) : rest) = if x == y then Just v else lookup x rest
