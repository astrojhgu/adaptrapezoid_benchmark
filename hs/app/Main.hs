module Main where

import Lib
import Criterion.Main

foo :: Floating a => a -> a
foo x= sin (x*x)


main :: IO ()
main = do
  putStrLn "Validating precision"
  let precise_result=0.527038339761566009286263102166809763899326865179511011538
  let result=integrate foo [0.0::Double, 1.0, 2.0, sqrt (8*pi)] 1e-10
  putStr "for sorted integration diff="
  putStrLn $ show $ abs $ precise_result-result
  defaultMain [
    bgroup "integration" [ bench "neumaier"  $ whnf (integrate foo [0.0::Double, 1.0, 2.0, sqrt (8*pi)]) 1e-10
               ]
              ]
