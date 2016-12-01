module Flight where

-- | center of gravity envolope graph points from kilograms
pointsKgs :: 
  [(Double, Double)] 
  -> (Double, Double)
pointsKgs x = 
  (sumKgsMoment x / 1000, sumKgs x)

-- | center of gravity envolope graph points from pounds
pointsLbs :: 
  [(Double, Double)] 
  -> (Double, Double)
pointsLbs x = 
  (sumLbsMoment x / 1000, sumLbs x)

-- | convert kilograms to pounds on a list of tuple's first elements
mapKgsToLbs :: 
  [(Double,Double)] 
  -> [(Double,Double)]
mapKgsToLbs = 
  map (\(x,y) -> (x * lbsPerKg, y))

-- | sum of moment from kilograms and arm
sumKgsMoment :: 
  [(Double,Double)] 
  -> Double
sumKgsMoment = 
  sum . map (kgsToLbs . momentLbsArm)

-- | sum of moment from pounds and arm
sumLbsMoment :: 
  [(Double,Double)] 
  -> Double
sumLbsMoment = 
  sum . map momentLbsArm

-- | sum of pounds on the list of tuple's first elements, given kilograms
sumKgs :: 
  [(Double,Double)] 
  -> Double
sumKgs = 
  sumLbs . mapKgsToLbs

-- | sum of pounds on the list of tuple's first elements
sumLbs :: 
  [(Double,Double)] -> Double
sumLbs = 
  sum . map fst

{-

╔════════╦════════╦═════════╦═════════╗
║ kg (a) ║ lb (b) ║ arm (c) ║ mom (d) ║
╚════════╩════════╩═════════╩═════════╝

a -> b => Kilograms to pounds
(b,c) -> d => Moment given pounds and arm
(a, c) -> d => Moment given kilograms and arm

-}

-- | a -> b
kgsToLbs :: 
  Double 
  -> Double
kgsToLbs kgs = 
  kgs * lbsPerKg

-- | (b,c) -> d
momentLbsArm :: 
  (Double, Double) 
  -> Double
momentLbsArm = 
  uncurry (*)

-- | (a, c) -> d
momentKgsArm :: 
  Double 
  -> Double 
  -> Double
momentKgsArm weight arm = 
  kgsToLbs weight * arm

-- | pounds per kilogram
lbsPerKg = 2.20462