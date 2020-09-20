module Main
import Data.List


data Point: (a: Type) -> Type where
  MkPoint: a -> a -> Point a

midpoint: (Fractional a)=>(a->a)->(Point a)->(Point a)->(Point a)
midpoint func (MkPoint x1 _ ) (MkPoint x2 _) = MkPoint xm fm where
  xm: a
  xm = (x1+x2)/(the a 2)
  fm: a
  fm = func xm

area: (Fractional a, Neg a)=>(Point a) -> (Point a) -> a
area (MkPoint x1 f1) (MkPoint x2 f2) = (x2-x1)*(f2+f1)/fromInteger 2

neumaier_sum: (Abs a, Ord a, Neg a)=>a->a->a->(a, a)
neumaier_sum x s c = (new_s, new_c) where
  mutual
    t: a
    t = s + x
    new_c : a
    new_c = c + (update_comp x s t)
    new_s : a
    new_s = t
    update_comp: a->a->a->a
    update_comp x' s' t' = if (abs s') > (abs x') then (s'-t')+x' else (x'-t')+s'

integrate_iter: (Fractional a, Abs a, Ord a, Neg a)=>(a->a)->a->(List (Point a)) -> a-> a->((List (Point a)), a, a)
integrate_iter func eps (left@(MkPoint _ f1)::right@(MkPoint _ f2)::others) old_area old_comp=
  let (MkPoint xm fm) = midpoint func left right
      (area', comp') = neumaier_sum (area left right) old_area old_comp
      in if abs (f1+f2-fm*2) <= eps
          then integrate_iter func eps (right::others) area' comp'
            else integrate_iter func eps (left::(MkPoint xm fm)::right::others) old_area old_comp
integrate_iter _ _ points old_area old_comp = (points , old_area, old_comp)

integrate: (Abs a, Fractional a, Ord a, Neg a) => (a->a) -> (ticks: List a) -> {auto ok: NonEmpty ticks} -> a -> a
integrate func ticks eps = let points= (map (\x => (MkPoint x (func x))) ticks)
                               eps' = eps * (fromInteger 4)/((last ticks)-(head ticks))
                               (_,sum_area,comp)=integrate_iter func eps' points 0 0
                                in sum_area + comp

foo: Double -> Double
foo x = sin (x*x)

main: IO()
main = do
  let a=integrate foo [0.0, 1.0, 2.0, sqrt (8.0*pi)] 1.0e-10
  printLn a
