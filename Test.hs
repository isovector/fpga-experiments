{-# LANGUAGE ApplicativeDo                        #-}
{-# LANGUAGE DeriveFunctor                        #-}
{-# LANGUAGE DerivingVia                          #-}
{-# LANGUAGE GADTs                                #-}
{-# LANGUAGE LambdaCase                           #-}
{-# LANGUAGE PartialTypeSignatures                #-}
{-# LANGUAGE StandaloneDeriving                   #-}
{-# LANGUAGE StandaloneKindSignatures             #-}
{-# LANGUAGE TupleSections                        #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test where

import Data.Functor
import Data.Functor.Compose
import Data.Bool (bool)
import Data.Maybe
import Clash.Prelude
import Clash.Annotations.TH


topEntity
  :: "clk" ::: Clock System
  -> VGAOut
topEntity clk = myEntity clk resetGen enableGen


data VGAOut = VGAOut
  { vo_hsync :: "hsync" ::: Signal System Bool
  , vo_vsync :: "vsync" ::: Signal System Bool
  , vo_gnd :: "gnd" ::: Signal System Bit
  , vo_all_gnd :: "all_gnd" ::: Signal System Bit
  , vo_red :: "red" ::: Signal System Bit
  , vo_green :: "green" ::: Signal System Bit
  , vo_blue :: "blue" ::: Signal System Bit
  , vo_yvis :: "yvis" ::: Signal System Bit
  }


myEntity
  :: "clk" ::: Clock System
  -> "rst" ::: Reset System
  -> "en" ::: Enable System
  -> VGAOut
myEntity = exposeClockResetEnable $ VGAOut hsync vsync (pure low) (pure low) red test (pure low) (fmap (boolToBit . isJust) ypos)

strengthen :: forall n k. _ => Index (n + k) -> Maybe (Index n)
strengthen ix
  | ix <= fromIntegral (maxBound @(Index n)) = Just $ fromIntegral ix
strengthen _ = Nothing

countWhen :: (1 <= n, KnownNat n, HiddenClockResetEnable dom) => Signal dom Bool -> (Signal dom (Index n), Signal dom Bool)
countWhen enable =
  let r = regEn 0 enable $ fmap (satAdd SatWrap 1) r
   in (r, r .==. pure maxBound)

count :: (1 <= n, KnownNat n, HiddenClockResetEnable dom) => (Signal dom (Index n), Signal dom Bool)
count = countWhen $ pure True

red :: _ => Signal dom Bit
red = visibly (pure low) $ fmap (maybe low (boolToBit . between (150, 250))) ypos

green :: _ => Signal dom Bit
green = visibly (pure low) $ fmap (boolToBit . isJust) ypos


between :: Ord a => (a, a) -> a -> Bool
between (lo, hi)a = lo <= a && a <= hi


column :: _ => Signal dom (Index 755)
colEnd :: _ => Signal dom Bool
(column, colEnd) = count

row :: _ => Signal dom (Index 492)
row = fst $ countWhen colEnd

hsync :: _ => Signal dom Bool
hsync = fmap (between (656, 751)) column

vsync :: _ => Signal dom Bool
vsync = fmap (between (491, 492)) row

xpos :: _ => Signal dom (Maybe (Index 640))
xpos = fmap strengthen column

ypos :: _ => Signal dom (Maybe (Index 480))
ypos = fmap strengthen row

visible :: _ => Signal dom Bool
visible = liftA2 (\a b -> isJust a && isJust b) xpos ypos

visibly :: _ => Signal dom a -> Signal dom a -> Signal dom a
visibly def sa = liftA3 (\b d a -> bool d a b) visible def sa

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
  deriving stock (Functor)
  deriving Applicative via Compose m Maybe



test :: _ => Signal dom Bit
test = fmap (fromMaybe low) $ runMaybeT $ do
  x <- MaybeT xpos
  y <- MaybeT ypos
  pure $ boolToBit $ x <= fromIntegral y


--   do
--   x <- maybe _ _ xpos
--   y <- maybe _ _ ypos
--



makeTopEntity 'topEntity
