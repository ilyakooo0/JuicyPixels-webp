{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.EncodeMode
  ( encodeYMode,
    encodeUVMode,
  )
where

import Codec.Picture.WebP.Internal.VP8.BoolEncoder
import Data.Word

-- | Encode Y mode (0-3) to bitstream
-- Mode mapping: 0=DC_PRED, 1=V_PRED, 2=H_PRED, 3=TM_PRED
-- Tree from RFC 6386: B_PRED="0", DC_PRED="100", V_PRED="101", H_PRED="110", TM_PRED="111"
-- Probs: [145, 156, 163, 128] for each decision point
encodeYMode :: Int -> BoolEncoder -> BoolEncoder
encodeYMode mode enc =
  case mode of
    0 ->
      -- DC_PRED: bits "100" (True, False, False)
      let enc1 = boolWrite 145 True enc -- bit 0: not B_PRED
          enc2 = boolWrite 156 False enc1 -- bit 1: go left (to DC/V node)
          enc3 = boolWrite 163 False enc2 -- bit 2: DC_PRED (left)
       in enc3
    1 ->
      -- V_PRED: bits "101" (True, False, True)
      let enc1 = boolWrite 145 True enc -- bit 0: not B_PRED
          enc2 = boolWrite 156 False enc1 -- bit 1: go left (to DC/V node)
          enc3 = boolWrite 163 True enc2 -- bit 2: V_PRED (right)
       in enc3
    2 ->
      -- H_PRED: bits "110" (True, True, False)
      let enc1 = boolWrite 145 True enc -- bit 0: not B_PRED
          enc2 = boolWrite 156 True enc1 -- bit 1: go right (to H/TM node)
          enc3 = boolWrite 128 False enc2 -- bit 2: H_PRED (left)
       in enc3
    3 ->
      -- TM_PRED: bits "111" (True, True, True)
      let enc1 = boolWrite 145 True enc -- bit 0: not B_PRED
          enc2 = boolWrite 156 True enc1 -- bit 1: go right (to H/TM node)
          enc3 = boolWrite 128 True enc2 -- bit 2: TM_PRED (right)
       in enc3
    _ -> enc -- Invalid mode, no change

-- | Encode UV mode (0-3) to bitstream
-- Mode mapping: 0=DC_PRED, 1=V_PRED, 2=H_PRED, 3=TM_PRED
-- Tree from RFC 6386: DC_PRED="0", V_PRED="10", H_PRED="110", TM_PRED="111"
-- Probs: [142, 114, 183] for each decision point
encodeUVMode :: Int -> BoolEncoder -> BoolEncoder
encodeUVMode mode enc =
  case mode of
    0 ->
      -- DC_PRED: bits "0" (False)
      boolWrite 142 False enc
    1 ->
      -- V_PRED: bits "10" (True, False)
      let enc1 = boolWrite 142 True enc -- bit 0: not DC_PRED
          enc2 = boolWrite 114 False enc1 -- bit 1: V_PRED (left)
       in enc2
    2 ->
      -- H_PRED: bits "110" (True, True, False)
      let enc1 = boolWrite 142 True enc -- bit 0: not DC_PRED
          enc2 = boolWrite 114 True enc1 -- bit 1: go right (to H/TM node)
          enc3 = boolWrite 183 False enc2 -- bit 2: H_PRED (left)
       in enc3
    3 ->
      -- TM_PRED: bits "111" (True, True, True)
      let enc1 = boolWrite 142 True enc -- bit 0: not DC_PRED
          enc2 = boolWrite 114 True enc1 -- bit 1: go right (to H/TM node)
          enc3 = boolWrite 183 True enc2 -- bit 2: TM_PRED (right)
       in enc3
    _ -> enc
