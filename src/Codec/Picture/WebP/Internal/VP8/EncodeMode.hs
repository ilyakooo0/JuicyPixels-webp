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
-- Tree values: 1=DC_PRED, 2=V_PRED, 3=H_PRED, 4=TM_PRED (add 1)
encodeYMode :: Int -> BoolEncoder -> BoolEncoder
encodeYMode mode enc =
  case mode of
    0 -> -- DC_PRED: tree value 1, bits "10"
         let enc1 = boolWrite 145 True enc   -- First bit: 1
             enc2 = boolWrite 156 False enc1 -- Second bit: 0
          in enc2
    1 -> -- V_PRED: tree value 2, bits "110"
         let enc1 = boolWrite 145 True enc   -- First bit: 1
             enc2 = boolWrite 156 True enc1  -- Second bit: 1
             enc3 = boolWrite 163 False enc2 -- Third bit: 0
          in enc3
    2 -> -- H_PRED: tree value 3, bits "1110"
         let enc1 = boolWrite 145 True enc   -- 1
             enc2 = boolWrite 156 True enc1  -- 1
             enc3 = boolWrite 163 True enc2  -- 1
             enc4 = boolWrite 128 False enc3 -- 0
          in enc4
    3 -> -- TM_PRED: tree value 4, bits "1111"
         let enc1 = boolWrite 145 True enc   -- 1
             enc2 = boolWrite 156 True enc1  -- 1
             enc3 = boolWrite 163 True enc2  -- 1
             enc4 = boolWrite 128 True enc3  -- 1
          in enc4
    _ -> enc -- Invalid mode, no change

-- | Encode UV mode (0-3) to bitstream
-- Same modes as Y: 0=DC_PRED, 1=V_PRED, 2=H_PRED, 3=TM_PRED
-- Tree values: 1=DC_PRED, 2=V_PRED, 3=H_PRED, 4=TM_PRED
encodeUVMode :: Int -> BoolEncoder -> BoolEncoder
encodeUVMode mode enc =
  case mode of
    0 -> -- DC_PRED: tree value 1, bits "10"
         let enc1 = boolWrite 142 True enc   -- First bit: 1
             enc2 = boolWrite 114 False enc1 -- Second bit: 0
          in enc2
    1 -> -- V_PRED: tree value 2, bits "110"
         let enc1 = boolWrite 142 True enc   -- 1
             enc2 = boolWrite 114 True enc1  -- 1
             enc3 = boolWrite 183 False enc2 -- 0
          in enc3
    2 -> -- H_PRED: tree value 3, bits "1110"
         let enc1 = boolWrite 142 True enc   -- 1
             enc2 = boolWrite 114 True enc1  -- 1
             enc3 = boolWrite 183 True enc2  -- 1
             enc4 = boolWrite 128 False enc3 -- 0 (using prob 128 for last bit)
          in enc4
    3 -> -- TM_PRED: tree value 4, bits "1111"
         let enc1 = boolWrite 142 True enc   -- 1
             enc2 = boolWrite 114 True enc1  -- 1
             enc3 = boolWrite 183 True enc2  -- 1
             enc4 = boolWrite 128 True enc3  -- 1
          in enc4
    _ -> enc
