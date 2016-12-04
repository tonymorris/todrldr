{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Arm =
  Arm {
    _armmeasure ::
      Int -- inches
  , _armrange ::
      Maybe (Int, Int)
  , _name ::
      Maybe String
  } deriving (Eq, Ord, Show)

armnorange :: 
  Int
  -> Maybe String
  -> Arm
armnorange n m =
  Arm n Nothing m

data AircraftArm =
  AircraftArm {
    _aeroplane ::
      Arm
  , _frontseat ::
      Arm
  , _fuel ::
      Arm
  , _rearseat ::
      Arm
  , _baggagea ::
      Arm
  , _baggageb ::
      Arm
  }
  deriving (Eq, Ord, Show)

c172arm ::
  Arm -- aeroplane arm
  -> AircraftArm
c172arm a =
  AircraftArm
    a
    (Arm 37 (Just (34, 46)) (Just "front seat"))
    (armnorange 48 (Just "fuel"))
    (armnorange 73 (Just "rear seat"))
    (Arm 95 (Just (82, 108)) (Just "baggage A"))
    (Arm 123 (Just (108, 142)) (Just "baggage B"))

-- baggage "A" maximum 120lb
-- baggage "B" maximum 50lb
-- maximum overall baggage 120lb

data AircraftWeight =
  AircraftWeight {
    _bew ::
      Double
  , _frontseatweight ::
      Double -- pounds
  , _fuelweight ::
      Double -- gallons
  , _rearseatweight ::
      Double
  , _baggageaweight ::
      Double
  , _baggagebweight ::
      Double
  }
  deriving (Eq, Ord, Show)

data MaximumWeight =
  MaximumWeight {
    baggagea ::
      Int
  , baggageb ::
      Int
  , totalbaggage ::
      Int
  , fuel ::
      Int
  , mrw ::
      Int
  , mtow ::
      Int
  }
  deriving (Eq, Ord, Show)
    
c172RMaximumWeight ::
  MaximumWeight
c172RMaximumWeight =
  MaximumWeight
    120
    50
    120
    336
    2558
    2550

sample =
  AircraftWeight
    1691.6
    363.763
    30
    176.37
    22.0462
    2.20462

-- https://hackage.haskell.org/package/hgeometry-0.5.0.0/docs/Data-Geometry-Polygon.html

-- AircraftArm -> MaximumWeight -> AircraftWeight -> CGEnvelope -> Report

{-
Weight (lbs): 1663.2
Arm (in): 40.719
Moment (lb-in): 67724
Basic Empty Weight

    Weight (lbs): 1691.6
    Arm (in): 40.600
    Moment (lb-in): 68679
-}