{-# LANGUAGE FlexibleContexts #-}

import Plots
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import Data.Typeable

_leftline ::
  [(Double, Double)]
_leftline = 
  [(52.5,1500), (68,1950), (104.5, 2550)]

_rightline ::
  [(Double, Double)]
_rightline = 
  [(120.5, 2550), (71, 1500)]

_polygon2 ::
  [(Double, Double)]
_polygon2 = 
  [(61, 1500), (89, 2200), (83, 2200)]

myaxis :: 
  Axis B V2 Double
myaxis = r2Axis &~ do

  linePlot _leftline $ do
    plotColor .= black

  linePlot _rightline $ do
    plotColor .= black

  linePlot _polygon2 $ do
    plotColor .= black


main :: 
  IO ()
main = 
  r2AxisMain myaxis