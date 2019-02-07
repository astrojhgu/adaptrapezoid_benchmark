module Main where

import Lib
import Criterion.Main

foo :: Floating a => a -> a
foo x= sin (x*x)


main :: IO ()
main = do
  putStrLn "Validating precision"
  let precise_result=0.527038339761566009286263102166809763899326865179511011538
  let result_sort=integrate foo [0.0::Double, 1.0, 2.0, sqrt (8*pi)] 1e-10
  putStr "for sorted integration diff="
  putStrLn $ show $ abs $ precise_result-result_sort
  let result_nosort=integrate_nosort foo [0.0::Double, 1.0, 2.0, sqrt (8*pi)] 1e-10
  putStr "for non-sorted integration diff="
  putStrLn $ show $ abs $ precise_result-result_nosort
  defaultMain [
    bgroup "integration" [ bench "sort"  $ whnf (integrate foo [0.0::Double, 1.0, 2.0, sqrt (8*pi)]) 1e-10
               , bench "no sort"  $ whnf (integrate_nosort foo [0.0::Double, 1.0, 2.0, sqrt (8*pi)]) 1e-10
               ]
              ]
