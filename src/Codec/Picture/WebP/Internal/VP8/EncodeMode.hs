{-# LANGUAGE BangPatterns #-}

module Codec.Picture.WebP.Internal.VP8.EncodeMode
  ( encodeYMode,
    encodeYModeBPred,
    encodeBSubMode,
    encodeUVMode,
  )
where

import Codec.Picture.WebP.Internal.VP8.BoolEncoder
import Codec.Picture.WebP.Internal.VP8.Tables (kfBmodeProbs)
import qualified Data.Vector.Unboxed as VU
import Data.Word

-- | Encode Y mode (0-3) to bitstream
-- Mode mapping: 0=DC_PRED, 1=V_PRED, 2=H_PRED, 3=TM_PRED
-- Tree from RFC 6386: B_PRED="0", DC_PRED="100", V_PRED="101", H_PRED="110", TM_PRED="111"
-- Probs: [145, 156, 163, 128] per libwebp (DC/V uses 163, H/TM uses 128)
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
          enc3 = boolWrite 128 False enc2 -- bit 2: H_PRED (left), prob 128 per libwebp
       in enc3
    3 ->
      -- TM_PRED: bits "111" (True, True, True)
      let enc1 = boolWrite 145 True enc -- bit 0: not B_PRED
          enc2 = boolWrite 156 True enc1 -- bit 1: go right (to H/TM node)
          enc3 = boolWrite 128 True enc2 -- bit 2: TM_PRED (right), prob 128 per libwebp
       in enc3
    _ -> enc -- Invalid mode, no change

-- | Encode Y mode as B_PRED (code "0" in kf_ymode_tree, prob 145)
{-# INLINE encodeYModeBPred #-}
encodeYModeBPred :: BoolEncoder -> BoolEncoder
encodeYModeBPred enc = boolWrite 145 False enc

-- | Encode a 4x4 sub-block intra mode using kfBmodeProbs[above][left] context.
-- Tree from RFC 6386 kf_bmode_tree:
--   B_DC=0 "0", B_TM=1 "10", B_VE=2 "110",
--   B_HE=3 "11100", B_LD=4 "11110", B_RD=5 "111010",
--   B_VR=6 "111011", B_VL=7 "111110", B_HD=8 "1111110", B_HU=9 "1111111"
-- 9 decision nodes → 9 probabilities per context (above*90 + left*9 + nodeIdx)
{-# INLINE encodeBSubMode #-}
encodeBSubMode :: Int -> Int -> Int -> BoolEncoder -> BoolEncoder
encodeBSubMode !aboveMode !leftMode !subMode !enc =
  let !probBase = aboveMode * 90 + leftMode * 9
      !p0 = kfBmodeProbs VU.! probBase
      !p1 = kfBmodeProbs VU.! (probBase + 1)
      !p2 = kfBmodeProbs VU.! (probBase + 2)
      !p3 = kfBmodeProbs VU.! (probBase + 3)
      !p4 = kfBmodeProbs VU.! (probBase + 4)
      !p5 = kfBmodeProbs VU.! (probBase + 5)
      !p6 = kfBmodeProbs VU.! (probBase + 6)
      !p7 = kfBmodeProbs VU.! (probBase + 7)
      !p8 = kfBmodeProbs VU.! (probBase + 8)
   in case subMode of
        0 ->
          -- B_DC_PRED: node0=False
          boolWrite p0 False enc
        1 ->
          -- B_TM_PRED: node0=True, node2=False
          let e1 = boolWrite p0 True enc
           in boolWrite p1 False e1
        2 ->
          -- B_VE_PRED: node0=T, node2=T, node4=False
          let e1 = boolWrite p0 True enc
              e2 = boolWrite p1 True e1
           in boolWrite p2 False e2
        3 ->
          -- B_HE_PRED: node0=T, node2=T, node4=T, node6=F, node8=F
          let e1 = boolWrite p0 True enc
              e2 = boolWrite p1 True e1
              e3 = boolWrite p2 True e2
              e4 = boolWrite p3 False e3
           in boolWrite p4 False e4
        4 ->
          -- B_LD_PRED: node0=T, node2=T, node4=T, node6=T, node12=F
          let e1 = boolWrite p0 True enc
              e2 = boolWrite p1 True e1
              e3 = boolWrite p2 True e2
              e4 = boolWrite p3 True e3
           in boolWrite p6 False e4
        5 ->
          -- B_RD_PRED: ..., node6=F, node8=T, node10=F
          let e1 = boolWrite p0 True enc
              e2 = boolWrite p1 True e1
              e3 = boolWrite p2 True e2
              e4 = boolWrite p3 False e3
              e5 = boolWrite p4 True e4
           in boolWrite p5 False e5
        6 ->
          -- B_VR_PRED: ..., node6=F, node8=T, node10=T
          let e1 = boolWrite p0 True enc
              e2 = boolWrite p1 True e1
              e3 = boolWrite p2 True e2
              e4 = boolWrite p3 False e3
              e5 = boolWrite p4 True e4
           in boolWrite p5 True e5
        7 ->
          -- B_VL_PRED: ..., node6=T, node12=T, node14=F
          let e1 = boolWrite p0 True enc
              e2 = boolWrite p1 True e1
              e3 = boolWrite p2 True e2
              e4 = boolWrite p3 True e3
              e5 = boolWrite p6 True e4
           in boolWrite p7 False e5
        8 ->
          -- B_HD_PRED: ..., node6=T, node12=T, node14=T, node16=F
          let e1 = boolWrite p0 True enc
              e2 = boolWrite p1 True e1
              e3 = boolWrite p2 True e2
              e4 = boolWrite p3 True e3
              e5 = boolWrite p6 True e4
              e6 = boolWrite p7 True e5
           in boolWrite p8 False e6
        9 ->
          -- B_HU_PRED: ..., node6=T, node12=T, node14=T, node16=T
          let e1 = boolWrite p0 True enc
              e2 = boolWrite p1 True e1
              e3 = boolWrite p2 True e2
              e4 = boolWrite p3 True e3
              e5 = boolWrite p6 True e4
              e6 = boolWrite p7 True e5
           in boolWrite p8 True e6
        _ -> enc

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
