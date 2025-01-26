{-# LANGUAGE TupleSections #-}

module Test where

import Clash.Prelude
import Clash.Annotations.TH


topEntity
  :: "clk" ::: Clock System
  -> ( "out0" ::: Signal System Bit
     , "out1" ::: Signal System Bit
     )
topEntity clk = unbundle $ myEntity clk resetGen enableGen


myEntity
  :: "clk" ::: Clock System
  -> "rst" ::: Reset System
  -> "en" ::: Enable System
  -> Signal System (Bit, Bit)
myEntity =
  exposeClockResetEnable $ fmap ((, low) . boolToBit) $ oscillate False $ SNat @27000000


makeTopEntity 'topEntity
