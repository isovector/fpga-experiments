{-# LANGUAGE ApplicativeDo                        #-}
{-# LANGUAGE DeriveFunctor                        #-}
{-# LANGUAGE DerivingVia                          #-}
{-# LANGUAGE GADTs                                #-}
{-# LANGUAGE LambdaCase                           #-}
{-# LANGUAGE PartialTypeSignatures                #-}
{-# LANGUAGE RecordWildCards                      #-}
{-# LANGUAGE StandaloneDeriving                   #-}
{-# LANGUAGE StandaloneKindSignatures             #-}
{-# LANGUAGE TupleSections                        #-}
{-# LANGUAGE ViewPatterns                         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test where

import Data.Functor
import Data.Functor.Compose
import Data.Bool (bool)
import Data.Maybe
import Clash.Prelude
import Clash.Annotations.TH

-- B13  A14
-- B14  A15
-- D14  E15
-- F16  F14
-- G15  G14
-- J14  J16
-- G12  F13
-- M14  M15
-- T14  R13
-- P13  R12
-- RED MARKER


topEntity
  :: "p1_up" ::: Signal System Bool
  -> "p1_down" ::: Signal System Bool
  -> "p2_up" ::: Signal System Bool
  -> "p2_down" ::: Signal System Bool
  -> "clk" ::: Clock System
  -> Signal System VGAOut
topEntity p1_up p1_down p2_up p2_down clk = myEntity p1_up p1_down p2_up p2_down clk resetGen enableGen


myEntity
  :: "p1_up" ::: Signal System Bool
  -> "p1_down" ::: Signal System Bool
  -> "p2_up" ::: Signal System Bool
  -> "p2_down" ::: Signal System Bool
  -> "clk" ::: Clock System
  -> "rst" ::: Reset System
  -> "en" ::: Enable System
  -> Signal System VGAOut
myEntity p1_up p1_down p2_up p2_down = exposeClockResetEnable $ do
  vo_hsync <- hsync
  vo_vsync <- vsync
  vo_gnd <- pure low
  vo_hot <- pure high
  vo_all_gnd <- pure low
  (vo_color, vo_buzzer) <- game $
    liftA2 (,)
      (Frame
            <$> newFrame
            <*> runMaybeT (liftA2 V2 (MaybeT xpos) (MaybeT ypos))
      )
      (Input <$> (parsePaddle <$> p1_up <*> p1_down)
             <*> (parsePaddle <$> p2_up <*> p2_down))
  pure VGAOut{..}

parsePaddle :: Bool -> Bool -> InputDir
parsePaddle True _ = MoveUp
parsePaddle _ True = MoveDown
parsePaddle _ _ = Neutral

data V2 w h = V2
  { p_x :: "x" ::: Index w
  , p_y :: "y" ::: Index h
  }
  deriving (Generic, NFDataX)


data GameState = GameState
  { gs_p1 :: Index 480
  , gs_p2 :: Index 480
  , gs_ballpos :: V2 640 480
  , gs_ballspeed :: V2 640 480
  , gs_balldir :: (Dir, Dir)
  , gs_beep :: Bool
  }
  deriving (Generic, NFDataX)

data Dir = Add | Sub
  deriving (Generic, NFDataX, Eq)

data InputDir = Neutral | MoveUp | MoveDown
  deriving (Generic, NFDataX)

data Frame = Frame
  { f_update :: Bool
  , f_drawPos :: Maybe (V2 640 480)
  }
  deriving (Generic, NFDataX)

data Input = Input
  { i_p1 :: InputDir
  , i_p2 :: InputDir
  }
  deriving (Generic, NFDataX)


addVec :: _ => (Dir, Dir) -> V2 x y -> V2 x y -> V2 x y
addVec (fx, fy) (V2 dx dy) (V2 x y) =
  V2
    (bool satSub satAdd (fx == Add) SatBound x dx)
    (bool satSub satAdd (fy == Add) SatBound y dy)


p1x :: Index 640
p1x = 50

p2x :: Index 640
p2x = 590

game :: _ => Signal dom (Frame, Input) -> Signal dom (Color, Bool)
game =
  flip mealy (GameState 240 240 (V2 320 240) (V2 5 5) (Add, Add) False) $ \gs (frame, i) ->
    ( case f_update frame of
        True -> update i gs
        False -> gs
    , case f_drawPos frame of
        Just (V2 x y) -> (draw gs x y, gs_beep gs)
        Nothing -> (Color 0 0 0, False)
    )

update :: Input -> GameState -> GameState
update Input{..} gs@GameState{..} = do
  let gs_p1' = movePaddle i_p1 gs_p1
      gs_p2' = movePaddle i_p2 gs_p2
      gs_ballpos' = addVec gs_balldir gs_ballspeed gs_ballpos
      dir' = reflect gs_ballpos' gs_balldir
      dir'' = bounce gs_ballpos' (V2 p1x gs_p1') Sub dir'
      dir''' = bounce gs_ballpos' (V2 p2x gs_p2') Add dir''

  gs
    { gs_p1 = gs_p1'
    , gs_p2 = gs_p2'
    , gs_ballpos = gs_ballpos'
    , gs_balldir = dir'''
    , gs_beep = gs_balldir /= dir'''
    }

bounce :: V2 640 480 -> (V2 640 480) -> Dir -> (Dir, Dir) -> (Dir, Dir)
bounce (V2 bx by) (V2 px py) d bd@(bdx, bdy)
  | d == bdx
  , around px 10 bx
  , around py 30 by
  = (flipDir bdx, bdy)
  | otherwise = bd

reflect :: V2 640 480 -> (Dir, Dir) -> (Dir, Dir)
reflect (V2 x y) (dx, dy)
  = ( bool id flipDir (x == minBound || x == maxBound) dx
    , bool id flipDir (y == minBound || y == maxBound) dy
    )

flipDir :: Dir -> Dir
flipDir Add = Sub
flipDir Sub = Add

movePaddle :: InputDir -> Index 480 -> Index 480
movePaddle Neutral x = x
movePaddle MoveUp x = satSub SatBound x 10
movePaddle MoveDown x = satAdd SatBound x 10

draw :: GameState -> Index 640 -> Index 480 -> Color
draw GameState{..} x y
  | around (p_x gs_ballpos) 5 x
  , around (p_y gs_ballpos) 5 y
  = black
  | around p1x 2 x
  , around gs_p1 30 y
  = yellow
  | around p2x 2 x
  , around gs_p2 30 y
  = yellow
  | otherwise
  = red

red :: Color
red = Color 1 0 0

pink :: Color
pink = Color 1 0 1

black :: Color
black = Color 0 0 0

yellow :: Color
yellow = Color 1 1 0


--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

countWhen :: (1 <= n, KnownNat n, HiddenClockResetEnable dom) => Signal dom Bool -> (Signal dom (Index n), Signal dom Bool)
countWhen enable =
  let r = regEn 0 enable $ fmap (satAdd SatWrap 1) r
   in (r, r .==. pure maxBound)


count :: (1 <= n, KnownNat n, HiddenClockResetEnable dom) => (Signal dom (Index n), Signal dom Bool)
count = countWhen $ pure True


between :: Ord a => (a, a) -> a -> Bool
between (lo, hi)a = lo <= a && a <= hi

around :: (Ord a, SaturatingNum a) => a -> a -> a -> Bool
around v d x = satSub SatBound v d <= x && x <= satAdd SatBound v d


strengthen :: forall n k. _ => Index (n + k) -> Maybe (Index n)
strengthen ix
  | ix <= fromIntegral (maxBound @(Index n)) = Just $ fromIntegral ix
strengthen _ = Nothing


--------------------------------------------------------------------------------
-- VGA
--------------------------------------------------------------------------------

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


newFrame :: _ => Signal dom Bool
newFrame = isRising False $ fmap isNothing ypos


data VGAOut = VGAOut
  { vo_hsync :: "hsync" ::: Bool
  , vo_vsync :: "vsync" ::: Bool
  , vo_hot :: "hot" ::: Bit
  , vo_gnd :: "gnd" ::: Bit
  , vo_all_gnd :: "all_gnd" ::: Bit
  , vo_color :: Color
  , vo_buzzer :: "buzzer" ::: Bool
  }


data Color = Color
  { c_red :: "red" ::: Bit
  , c_green :: "green" ::: Bit
  , c_blue :: "blue" ::: Bit
  }


newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
  deriving stock (Functor)
  deriving Applicative via Compose m Maybe


makeTopEntity 'topEntity
