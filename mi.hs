data Arm =
  Arm {
    armmeasure ::
      Int
  , armrange ::
      Maybe (Int, Int)
  } deriving (Eq, Ord, Show)

armnorange :: 
  Int
  -> Arm
armnorange n =
  Arm n Nothing

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
    (Arm 37 (Just (34, 46)))
    (armnorange 48)
    (armnorange 73)
    (Arm 95 (Just (82, 108)))
    (Arm 123 (Just (108, 142)))

-- baggage "A" maximum 120lb
-- baggage "B" maximum 50lb
-- maximum overall baggage 120lb

data WeightUnit =
  Pounds
  | Kilograms
  deriving (Eq, Ord, Show)      

data FuelWeightUnit =
  FuelWeight WeightUnit
  | Gallons
  | Litres
  deriving (Eq, Ord, Show)

data AircraftWeight =
  AircraftWeight {
    _bew ::
      (Double, WeightUnit)
  , _frontseatweight ::
      (Double, WeightUnit)
  , _fuelweight ::
      (Double, FuelWeightUnit)
  , _rearseatweight ::
      (Double, WeightUnit)
  , _baggageaweight ::
      (Double, WeightUnit)
  , _baggagebweight ::
      (Double, WeightUnit)
  }
  deriving (Eq, Ord, Show)

sample =
  AircraftWeight
    (770, Kilograms)
    (165, Kilograms)
    (30, Gallons)
    (80, Kilograms)
    (10, Kilograms)
    (1, Kilograms)
    