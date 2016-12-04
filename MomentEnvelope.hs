{-# LANGUAGE FlexibleContexts #-}

import Plots
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

import Data.Typeable

_polygon1 = [(120.5, 2550), (71, 1500), (52.5,1500), (68,1950), (104.5, 2550)]
_polygon2 = [(61, 1500), (89, 2200), (82.5, 2200)]


myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  linePlot' _polygon1
  linePlot' _polygon2

main :: IO ()
main = r2AxisMain myaxis