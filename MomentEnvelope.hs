{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where
import Diagrams.Prelude
import Diagrams.TwoD.Grid
import Diagrams.Backend.SVG.CmdLine

diagram :: Int -> Int -> Diagram B
diagram n m = 

  gridWithHalves n m #

  -- annotate y values
  annY (2*m) "1500" #
  annY  0    "2600" #

  -- annontate x values
  annX (2*n) "130" #
  annX  0    "50"

  where

    annY y val = annotate val txtPt black 0 y
    annX x val = annotate val txtPt black (x+1) (2*m+1)

    txtPt t = circle cSize # opacity 0.0 # lw none
              ===
              text t # fontSize (local 0.02)

    cSize :: Double
    cSize = 0.03


main :: IO ()
main = mainWith $ diagram 16 22