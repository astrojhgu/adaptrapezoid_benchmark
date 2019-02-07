{-# LANGUAGE BangPatterns #-}
module Lib (integrate)
    where

    data Point a=Point !a !a deriving Show -- x f

    midpoint::(Fractional a)=>(a->a)->(Point a)->(Point a)->(Point a)
    midpoint func (Point x1 _) (Point x2 _)=Point xm fm
        where 
            xm=(x1+x2)/2
            fm=func xm

    area::(Fractional a)=>(Point a)->(Point a)->a
    area (Point x1 f1) (Point x2 f2)=(f1+f2)*(x2-x1)/2

    {-# INLINE neumaier_sum #-}
    neumaier_sum::(Fractional a, Ord a)=>a->a->a->(a,a)
    neumaier_sum !x !s !c=(new_s, new_c)
            where
                t=s+x
                new_c = c + (update_comp x s t)
                new_s=t
                update_comp x1 s1 t1
                    | (abs s1) >= (abs x1) = (s1 - t1) + x1
                    | otherwise = (x1 - t1) + s1
                
    
    {-# INLINE integrate_iter #-}
    integrate_iter::(Fractional a, Ord a)=>(a->a)->a->[Point a]->a->a->([Point a], a, a)
    integrate_iter func eps (left@(Point _ f1):right@(Point _ f2):others) !old_area !old_comp
                | abs (f1+f2-fm*2) <= eps = integrate_iter func eps (right:others) area1 comp1
                | otherwise = integrate_iter func eps (left:(Point xm fm):right:others) old_area old_comp
                where 
                    (Point xm fm)=midpoint func left right
                    (area1, comp1)=neumaier_sum (area left right) old_area old_comp
    integrate_iter _ _ points old_area old_comp= (points, old_area, old_comp)
        
    {-# INLINABLE integrate #-}
    integrate::(Fractional a, Ord a)=>(a->a)->[a]->a->a
    integrate func ticks eps=sum_area+comp
            where 
                points=fmap (\x->Point x $ func x) ticks
                eps1=eps*4/((last ticks)-(head ticks))
                (_,sum_area,comp)=integrate_iter func eps1 points 0 0
    {-# SPECIALIZE integrate :: (Double -> Double) -> [Double] -> Double -> Double #-}
