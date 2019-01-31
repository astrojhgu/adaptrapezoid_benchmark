module Main where

import Lib
import Criterion.Main

foo x= sin (x*x)


main = defaultMain [
  bench "integrating sin(x^2)"  $ whnf (integrate foo [0.0, 1.0, 2.0, sqrt (8*pi)]) 1e-10
  ]