module CenterOfGravity where

data Result = Utility | Normal | Outside

locate ::
  Double
  -> Double
  -> Result
locate =
  undefined

-- |
--
-- >>> _inner 65
-- 1600
-- >>> _inner 71
-- 1750
_inner :: Int -> Maybe Int
_inner x = 
  if x < 89
    then Just (25*x-25)
    else Nothing