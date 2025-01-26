{-# LANGUAGE TupleSections #-}

module Test where

import Clash.Prelude


topEntity
  :: "clk" ::: Clock System
  -> Signal System (Bit, Bit)
topEntity clk = myEntity clk  (unsafeToReset $ pure False) (toEnable $ pure True)


myEntity
  :: "clk" ::: Clock System
  -> "rst" ::: Reset System
  -> "en" ::: Enable System
  -> Signal System (Bit, Bit)
myEntity =
  exposeClockResetEnable $ fmap ((, low) . boolToBit) $ oscillate False $ SNat @50000000
