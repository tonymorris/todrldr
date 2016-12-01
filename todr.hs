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


intervalsPressureAltitude ::
  PressureAltitude
  -> TakeOffDistance
  -> (TakeOffDistanceAltitude, TakeOffDistanceAltitude, Int)
intervalsPressureAltitude (PressureAltitude n) (TakeOffDistance _0 _1 _2 _3 _4 _5 _6 _7 _8) =
  let (d, m) =
        n `divMod` 1000
      (r, s) =
        case d of
          0 ->
            (_0, _1)
          1 ->
            (_1, _2)
          2 ->
            (_2, _3)
          3 ->
            (_3, _4)
          4 ->
            (_4, _5)
          5 ->
            (_5, _6)
          6 ->
            (_6, _7)
          7 ->
            (_7, _8)
          8 ->
            (_7, _8)
  in  (r, s, m)

intervalsTemperature ::
  Temperature
  -> TakeOffDistanceAltitude
  -> (Int, Int, Int)
intervalsTemperature (Temperature n) (TakeOffDistanceAltitude _0 _1 _2 _3 _4) =
  let (d, m) =
        n `divMod` 10
      (r, s) =
        case d of
          0 ->
            (_0, _1)
          1 ->
            (_1, _2)
          2 ->
            (_2, _3)
          3 ->
            (_3, _4)
          4 ->
            (_3, _4)          
  in  (r, s, m)


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

-- todr (PressureAltitude 100) (Temperature 10) c172s_2550lbs_groundroll_takeoff

-- |
--
-- >>> todr (PressureAltitude 3500) (Temperature 25) c172s_2550lbs_groundroll_takeoff
-- 1427.5
--
-- >>> todr (PressureAltitude 3000) (Temperature 20) c172s_2550lbs_groundroll_takeoff
-- 1310.0
--
-- >>> todr (PressureAltitude 3000) (Temperature 25) c172s_2550lbs_groundroll_takeoff
-- 1360.0
--
-- >>> todr (PressureAltitude 3500) (Temperature 20) c172s_2550lbs_groundroll_takeoff
-- 1375.0
--
-- >>> todr (PressureAltitude 3250) (Temperature 27) c172s_2550lbs_groundroll_takeoff
-- 1414.25
--
-- >>> todr (PressureAltitude 5130) (Temperature 14) c172s_2550lbs_groundroll_takeoff
-- 1533.02
todr ::
  PressureAltitude
  -> Temperature
  -> TakeOffDistance
  -> Double
todr e@(PressureAltitude pa) b@(Temperature temp) chart =
  let (x,  y,  q) = intervalsPressureAltitude e chart
      (x0, y0, r) = intervalsTemperature b x
      (x1, y1, _) = intervalsTemperature b y
      normpa g h  = fromIntegral ((h - g) * q) / 1000 + fromIntegral g
      normtp g h  = (h - g) * fromIntegral r / 10 + g
  in  normtp (normpa x0 x1) (normpa y0 y1)

  {-
  let (x_1, x_2) = intervalsPressureAltitude e chart
      (k_1, k_2) = intervalsTemperature b x_1
      (k_3, k_4) = intervalsTemperature b x_2
      a = tempLower b
      d = paLower e
  in  
    calculateTodr a b d e k_1 k_2 k_3 k_4
  -}

{-
calculateTodr :: 
  Double
  -> Temperature
  -> Double
  -> PressureAltitude
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
calculateTodr a b d e k_1 k_2 k_3 k_4 =
  ((b`tpMinusDouble`a)/10)*
  ((((e`paMinusDouble`d)/1000)*(k_4-k_2)+k_2) 
  - (((e`paMinusDouble`d)/1000)*(k_3 - k_1) + k_1))
  + (((e`paMinusDouble`d)/1000)*(k_3 - k_1) + k_1)

tpMinusDouble :: Temperature -> Double -> Double
tpMinusDouble (Temperature x) y = x - y

paMinusDouble :: PressureAltitude -> Double -> Double
paMinusDouble (PressureAltitude x) y = x - y

tempLower ::
  Temperature
  -> Double
tempLower (Temperature n)
  | n >= 0 && n < 10   = 0
  | n >= 10 && n < 20  = 10
  | n >= 20 && n < 30  = 20
  | n >= 30 && n <= 40 = 30

paLower ::
  PressureAltitude
  -> Double
paLower (PressureAltitude n)
  | n >= 0 && n < 1000     = 0
  | n >= 1000 && n < 2000  = 1000
  | n >= 2000 && n < 3000  = 2000
  | n >= 3000 && n < 4000  = 3000
  | n >= 4000 && n < 5000  = 4000
  | n >= 5000 && n < 6000  = 5000
  | n >= 6000 && n < 7000  = 6000
  | n >= 7000 && n <= 8000 = 7000

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
  -> (Double, Double)
intervalsTemperature (Temperature n) (TakeOffDistanceAltitude _0 _1 _2 _3 _4)
  | n >= 0 && n < 10   = (_0, _1)
  | n >= 10 && n < 20  = (_1, _2)
  | n >= 20 && n < 30  = (_2, _3)
  | n >= 30 && n <= 40 = (_3, _4)

example ::
  TakeOffDistance
  -> Double
example (TakeOffDistance _ _ _ (TakeOffDistanceAltitude _ x _ _ _) _ _ _ _ (TakeOffDistanceAltitude _ _ _ y _)) =
  x + y
-}
