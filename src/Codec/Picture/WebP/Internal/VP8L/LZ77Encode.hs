{-# LANGUAGE BangPatterns #-}

-- | LZ77 encoder for VP8L lossless compression.
-- Provides hash-chain match finding, prefix code utilities,
-- and the 2D distance map from the VP8L spec (RFC 9649).
module Codec.Picture.WebP.Internal.VP8L.LZ77Encode
  ( Token (..),
    lz77Compress,
    valueToPrefixCode,
    kDistanceMapXY,
    buildReverseDistanceMap,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Bits
import qualified Data.IntMap.Strict as IM
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word

-- | LZ77 token: either a literal pixel or a back-reference.
data Token
  = -- | Literal ARGB pixel
    TLiteral {-# UNPACK #-} !Word32
  | -- | Back-reference: (length in pixels, distance in pixels in scan-line order)
    TBackRef {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Show)

-- | The 120 (xi, yi) offset pairs for VP8L 2D distance codes (RFC 9649 Section 4.2.2).
-- Index i corresponds to distance code (i+1).
-- xi = column offset (positive = left in scan order), yi = row offset (positive = above).
-- Convert to scan-line distance: dist = xi + yi * image_width; if dist < 1 then dist = 1.
kDistanceMapXY :: VU.Vector (Int, Int)
kDistanceMapXY =
  VU.fromList
    [ (0, 1),
      (1, 0),
      (1, 1),
      (-1, 1),
      (0, 2),
      (2, 0),
      (1, 2),
      (-1, 2),
      (2, 1),
      (-2, 1),
      (2, 2),
      (-2, 2),
      (0, 3),
      (3, 0),
      (1, 3),
      (-1, 3),
      (3, 1),
      (-3, 1),
      (2, 3),
      (-2, 3),
      (3, 2),
      (-3, 2),
      (0, 4),
      (4, 0),
      (1, 4),
      (-1, 4),
      (4, 1),
      (-4, 1),
      (3, 3),
      (-3, 3),
      (2, 4),
      (-2, 4),
      (4, 2),
      (-4, 2),
      (0, 5),
      (3, 4),
      (-3, 4),
      (4, 3),
      (-4, 3),
      (5, 0),
      (1, 5),
      (-1, 5),
      (5, 1),
      (-5, 1),
      (2, 5),
      (-2, 5),
      (5, 2),
      (-5, 2),
      (4, 4),
      (-4, 4),
      (3, 5),
      (-3, 5),
      (5, 3),
      (-5, 3),
      (0, 6),
      (6, 0),
      (1, 6),
      (-1, 6),
      (6, 1),
      (-6, 1),
      (2, 6),
      (-2, 6),
      (6, 2),
      (-6, 2),
      (4, 5),
      (-4, 5),
      (5, 4),
      (-5, 4),
      (3, 6),
      (-3, 6),
      (6, 3),
      (-6, 3),
      (0, 7),
      (7, 0),
      (1, 7),
      (-1, 7),
      (5, 5),
      (-5, 5),
      (7, 1),
      (-7, 1),
      (4, 6),
      (-4, 6),
      (6, 4),
      (-6, 4),
      (2, 7),
      (-2, 7),
      (7, 2),
      (-7, 2),
      (3, 7),
      (-3, 7),
      (7, 3),
      (-7, 3),
      (5, 6),
      (-5, 6),
      (6, 5),
      (-6, 5),
      (8, 0),
      (4, 7),
      (-4, 7),
      (7, 4),
      (-7, 4),
      (8, 1),
      (8, 2),
      (6, 6),
      (-6, 6),
      (8, 3),
      (5, 7),
      (-5, 7),
      (7, 5),
      (-7, 5),
      (8, 4),
      (6, 7),
      (-6, 7),
      (7, 6),
      (-7, 6),
      (8, 5),
      (7, 7),
      (-7, 7),
      (8, 6),
      (8, 7)
    ]

-- | Build reverse distance map: scan-line distance -> distance code (1-120).
-- For a given image width, computes which 1D distances correspond to 2D codes.
-- Only includes entries where the 1D distance >= 1.
-- When multiple 2D codes map to the same 1D distance, keeps the smallest code.
buildReverseDistanceMap :: Int -> IM.IntMap Int
buildReverseDistanceMap width =
  VU.ifoldl'
    ( \acc i (xi, yi) ->
        let dist = xi + yi * width
            code = i + 1
         in if dist >= 1
              then IM.insertWith min dist code acc
              else acc
    )
    IM.empty
    kDistanceMapXY

-- | Convert a distance code to a VP8L prefix code.
-- Given a scan-line pixel distance and image width, returns the distance code
-- suitable for prefix encoding. Uses 2D codes (1-120) when possible.
distToDistCode :: IM.IntMap Int -> Int -> Int
distToDistCode reverseMap dist =
  case IM.lookup dist reverseMap of
    Just code2d -> code2d -- 2D code 1-120
    Nothing -> dist + 120 -- 1D code > 120

-- | Inverse of the VP8L prefix code formula (RFC 9649 Section 4.2.1).
-- Given a value (1-based), returns (prefixCode, extraBits, extraValue).
-- Used for both length encoding (value 1-4096, code 0-23) and
-- distance encoding (value 1-1048576, code 0-39).
--
-- The forward formula is:
--   if code < 4: value = code + 1
--   else: extra_bits = (code-2)>>1; offset = (2 + (code&1)) << extra_bits; value = offset + extra + 1
valueToPrefixCode :: Int -> (Int, Int, Int)
valueToPrefixCode val
  | val <= 4 = (val - 1, 0, 0)
  | otherwise =
      let !val' = val - 1
          !nBits = intLog2 val' -- floor(log2(val'))
          !extraBits = nBits - 1
          !halfBit = (val' `shiftR` extraBits) .&. 1
          !prefixCode = 2 * nBits + halfBit
          !base = (2 + halfBit) `shiftL` extraBits
          !extraValue = val' - base
       in (prefixCode, extraBits, extraValue)

-- | Floor of log base 2 for positive integers.
{-# INLINE intLog2 #-}
intLog2 :: Int -> Int
intLog2 n = finiteBitSize n - 1 - countLeadingZeros n

-- --------------------------------------------------------------------------
-- LZ77 Hash-Chain Match Finding
-- --------------------------------------------------------------------------

-- | Hash bits for the hash table (2^18 = 262144 entries).
hashBits :: Int
hashBits = 18

-- | Maximum chain depth to search.
maxChainDepth :: Int
maxChainDepth = 32

-- | Minimum match length in pixels.
minMatchLen :: Int
minMatchLen = 2

-- | Maximum match length in pixels (VP8L spec limit).
maxMatchLen :: Int
maxMatchLen = 4096

-- | Hash a single ARGB pixel to an index in [0, 2^hashBits - 1].
{-# INLINE hashPixel #-}
hashPixel :: Word32 -> Int
hashPixel px =
  fromIntegral ((0x1e35a7bd * px) `shiftR` (32 - hashBits))

-- | LZ77 compress pixel data using hash-chain match finding.
-- Returns a vector of tokens (literals and back-references).
lz77Compress :: Int -> Int -> VS.Vector Word32 -> V.Vector Token
lz77Compress _width _height pixels = runST $ do
  let !numPixels = VS.length pixels
      !hashSize = 1 `shiftL` hashBits

  -- Hash table: hash -> most recent position (-1 = empty)
  hashTable <- VUM.replicate hashSize (-1 :: Int)
  -- Chain table: position -> previous position with same hash (-1 = end of chain)
  chainTable <- VUM.replicate numPixels (-1 :: Int)

  -- Output: collect tokens in a list (reversed), then convert
  tokensRef <- newSTRef ([] :: [Token])
  countRef <- newSTRef (0 :: Int)

  let -- Insert a position into the hash chain
      {-# INLINE insertHash #-}
      insertHash !pos = do
        let !px = pixels `VS.unsafeIndex` pos
            !h = hashPixel px
        prev <- VUM.unsafeRead hashTable h
        VUM.unsafeWrite hashTable h pos
        VUM.unsafeWrite chainTable pos prev

      -- Emit a token
      {-# INLINE emit #-}
      emit !tok = do
        modifySTRef' tokensRef (tok :)
        modifySTRef' countRef (+ 1)

      -- Find the best match starting at pos
      findBest !pos = do
        let !px = pixels `VS.unsafeIndex` pos
            !h = hashPixel px
            !maxLen = min maxMatchLen (numPixels - pos)
        candidate <- VUM.unsafeRead hashTable h
        go candidate 0 0 0 maxLen
        where
          go !cand !bestLen !bestDist !depth !maxLen
            | cand < 0 || depth >= maxChainDepth = return (bestLen, bestDist)
            | otherwise = do
                let !dist = pos - cand
                -- Match: compare pixels starting at cand and pos
                let matchLen = countMatch cand pos maxLen
                if matchLen > bestLen
                  then do
                    next <- VUM.unsafeRead chainTable cand
                    go next matchLen dist (depth + 1) maxLen
                  else do
                    next <- VUM.unsafeRead chainTable cand
                    go next bestLen bestDist (depth + 1) maxLen

          -- Count matching pixels between two positions
          {-# INLINE countMatch #-}
          countMatch !src !dst !maxLen = goMatch 0
            where
              goMatch !i
                | i >= maxLen = i
                | (pixels `VS.unsafeIndex` (src + i)) /= (pixels `VS.unsafeIndex` (dst + i)) = i
                | otherwise = goMatch (i + 1)

      -- Main loop
      loop !pos
        | pos >= numPixels = return ()
        | otherwise = do
            (bestLen, bestDist) <- findBest pos

            if bestLen >= minMatchLen
              then do
                -- Insert current position into hash
                insertHash pos
                -- Insert remaining positions covered by the match
                forM_ [pos + 1 .. pos + bestLen - 1] $ \p ->
                  when (p < numPixels) $ insertHash p
                emit (TBackRef bestLen bestDist)
                loop (pos + bestLen)
              else do
                insertHash pos
                emit (TLiteral (pixels `VS.unsafeIndex` pos))
                loop (pos + 1)

  loop 0

  tokens <- readSTRef tokensRef
  return $! V.fromList (reverse tokens)
