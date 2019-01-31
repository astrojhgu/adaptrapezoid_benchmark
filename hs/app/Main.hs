module Main where

import Lib
import Criterion.Main

foo x= sin (x*x)


main = defaultMain [
  bgroup "integration" [ bench "sort"  $ whnf (integrate foo [0.0, 1.0, 2.0, sqrt (8*pi)]) 1e-10
               , bench "no sort"  $ whnf (integrate_nosort foo [0.0, 1.0, 2.0, sqrt (8*pi)]) 1e-10
               ]
  ]
  