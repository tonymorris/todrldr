{-# LANGUAGE TemplateHaskell #-}

module TODR(
  TakeOffDistanceAltitude(..)
, HasTakeOffDistanceAltitude(..)  
, TakeOffDistance(..)
, HasTakeOffDistance(..)
, c172s_2550lbs_groundroll_takeoff
, PressureAltitude
, AsPressureAltitude(..)
, todr
) where

import Control.Lens

data TakeOffDistanceAltitude =
  TakeOffDistanceAltitude {
    _temp00 ::
      Int
  , _temp10 ::
      Int
  , _temp20 ::
      Int
  , _temp30 ::
      Int
  , _temp40 ::
      Int
  }
  deriving (Eq, Ord, Show)

makeClassy ''TakeOffDistanceAltitude

data TakeOffDistance =
  TakeOffDistance {
    _pa0000 ::
      TakeOffDistanceAltitude
  , _pa1000 ::
      TakeOffDistanceAltitude
  , _pa2000 ::
      TakeOffDistanceAltitude
  , _pa3000 ::
      TakeOffDistanceAltitude
  , _pa4000 ::
      TakeOffDistanceAltitude
  , _pa5000 ::
      TakeOffDistanceAltitude
  , _pa6000 ::
      TakeOffDistanceAltitude
  , _pa7000 ::
      TakeOffDistanceAltitude
  , _pa8000 ::
      TakeOffDistanceAltitude
  }
  deriving (Eq, Ord, Show)

makeClassy ''TakeOffDistance

c172s_2550lbs_groundroll_takeoff ::
  TakeOffDistance
c172s_2550lbs_groundroll_takeoff =
  TakeOffDistance
    (
      TakeOffDistanceAltitude
        860
        925
        995
        1070
        1150
    )
    (
      TakeOffDistanceAltitude
        940
        1010
        1090
        1170
        1260
    )
    (
      TakeOffDistanceAltitude
        1025
        1110
        1195
        1285
        1380
    )
    (
      TakeOffDistanceAltitude
        1125
        1215
        1310
        1410
        1515
    )
    (
      TakeOffDistanceAltitude
        1235
        1335
        1440
        1550
        1660
    )
    (
      TakeOffDistanceAltitude
        1355
        1465
        1585
        1705
        1825
    )
    (
      TakeOffDistanceAltitude
        1495
        1615
        1745
        1875
        2010
    )
    (
      TakeOffDistanceAltitude
        1645
        1785
        1920
        2065
        2215
    )
    (
      TakeOffDistanceAltitude
        1820
        1970
        2120
        2280
        2450
    )

newtype PressureAltitude =
  PressureAltitude
    Int
  deriving (Eq, Ord, Show)

class AsPressureAltitude r0 where
  _PressureAltitude ::
    Prism' r0 PressureAltitude

instance AsPressureAltitude PressureAltitude where
  _PressureAltitude =
    id

instance AsPressureAltitude Int where
  _PressureAltitude =
    prism'
      (\(PressureAltitude n) -> n)
      (\n -> if n < 0 || n > 8000
               then
                 Nothing
              else
                 Just (PressureAltitude n))

newtype Temperature =
  Temperature
    Int
  deriving (Eq, Ord, Show)

class AsTemperature r0 where
  _Temperature ::
    Prism' r0 Temperature

instance AsTemperature Temperature where
  _Temperature =
    id

instance AsTemperature Int where
  _Temperature =
    prism'
      (\(Temperature n) -> n)
      (\n -> if n < 0 || n > 40
               then
                 Nothing
              else
                 Just (Temperature n))

todr ::
  PressureAltitude
  -> Temperature
  -> TakeOffDistance
  -> Double
todr pa@(PressureAltitude pa') temp@(Temperature temp') chart =
  let (pa1, pa2) = intervalsPressureAltitude pa chart
      (x1, y1)   = intervalsTemperature temp pa1
      (x2, y2)   = intervalsTemperature temp pa2
  in  undefined

{-

.---.-----.------.-----.
|   |  a  |  b   |  c  |
:---+-----+------+-----:
| d | k_1 |      | k_2 |
:---+-----+------+-----:
| e |     | todr |     |
:---+-----+------+-----:
| f | k_3 |      | k_4 |
'---'-----'------'-----'

-}

calculateTodr :: 
  Double -- a
  -> Double -- b
  -> Double -- d
  -> Double -- e
  -> Double -- k_1
  -> Double -- k_2
  -> Double -- k_3
  -> Double -- k_4
  -> Double -- todr
calculateTodr a b d e k_1 k_2 k_3 k_4 =
  ((b-a)/10)*
  ((((e-d)/1000)*(k_4-k_2)+k_2) 
  - (((e-d)/1000)*(k_3 - k_1) + k_1))
  + (((e-d)/1000)*(k_3 - k_1) + k_1)

intervalsPressureAltitude ::
  PressureAltitude
  -> TakeOffDistance
  -> (TakeOffDistanceAltitude, TakeOffDistanceAltitude)
intervalsPressureAltitude (PressureAltitude n) (TakeOffDistance _0 _1 _2 _3 _4 _5 _6 _7 _8) 
  | n >= 0 && n < 1000     = (_0, _1)
  | n >= 1000 && n < 2000  = (_1, _2)
  | n >= 2000 && n < 3000  = (_2, _3)
  | n >= 3000 && n < 4000  = (_3, _4)
  | n >= 4000 && n < 5000  = (_4, _5)
  | n >= 5000 && n < 6000  = (_5, _6)
  | n >= 6000 && n < 7000  = (_6, _7)
  | n >= 7000 && n <= 8000 = (_7, _8)

intervalsTemperature ::
  Temperature
  -> TakeOffDistanceAltitude
  -> (Int, Int)
intervalsTemperature (Temperature n) (TakeOffDistanceAltitude _0 _1 _2 _3 _4)
  | n >= 0 && n < 10   = (_0, _1)
  | n >= 10 && n < 20  = (_1, _2)
  | n >= 20 && n < 30  = (_2, _3)
  | n >= 30 && n <= 40 = (_3, _4)


example ::
  TakeOffDistance
  -> Int
example (TakeOffDistance _ _ _ (TakeOffDistanceAltitude _ x _ _ _) _ _ _ _ (TakeOffDistanceAltitude _ _ _ y _)) =
  x + y