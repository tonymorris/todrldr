module Quadratics where

{-

Quadratic functions for 0 degrees, 10 degrees... 40 degrees.
x is the pressure altitude (ft), and f(x) is ground roll

-}

_zero :: Double -> Double
_zero x = 4.2369*10**(-6)*x**2 + 0.0743*x + 860.1855

_ten :: Double -> Double
_ten x = 4.0505*10**(-6)*x**2 + 0.083*x + 924.5479

_two :: Double -> Double
_two x = 4.8599*10**(-6)*x**2 + 0.0894*x + 995.2157

_three :: Double -> Double
_three x = 5.6074*10**(-6)*x**2 + 0.0963*x + 1069.2814

_four :: Double -> Double
_four x = 4.9078*10**(-6)*x**2 + 0.1049*x + 1149.9793