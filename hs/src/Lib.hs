module Lib (integrate, integrate_nosort)
    where
    import Data.List

    data Point a=Point !a !a deriving Show -- x f

    midpoint::(Fractional a)=>(a->a)->(Point a)->(Point a)->(Point a)
    midpoint func (Point x1 _) (Point x2 _)=Point xm fm
        where 
            xm=(x1+x2)/(fromInteger 2)
            fm=func xm

    area::(Fractional a)=>(Point a)->(Point a)->a
    area (Point x1 f1) (Point x2 f2)=(f1+f2)*(x2-x1)/(fromInteger 2)
    

    {-# INLINE integrate_iter #-}
    integrate_iter::(Fractional a, Ord a)=>(a->a)->a->[Point a]->[a]->([Point a], [a])
    integrate_iter func eps (left@(Point _ f1):right@(Point _ f2):others) areas
                | abs (f1+f2-fm*(fromInteger 2)) <= eps = integrate_iter func eps (right:others) ((area left right):areas)
                | otherwise = integrate_iter func eps (left:(Point xm fm):right:others) areas
                where (Point xm fm)=midpoint func left right
    integrate_iter _ _ points areas = (points, areas)
        
    {-# INLINABLE integrate #-}
    integrate::(Fractional a, Ord a)=>(a->a)->[a]->a->a
    integrate func ticks eps=foldr (+) (fromInteger 0) sorted_areas
            where 
                points=fmap (\x->Point x $ func x) ticks
                eps1=eps*(fromInteger 4)/((last ticks)-(head ticks))
                areas=snd $ integrate_iter func eps1 points []
                sorted_areas=sortBy (\a b->compare (abs a) (abs b)) areas

    {-# INLINE integrate_iter_nosort #-}
    integrate_iter_nosort::(Fractional a, Ord a)=>(a->a)->a->[Point a]->a->([Point a], a)
    integrate_iter_nosort func eps (left@(Point _ f1):right@(Point _ f2):others) old_area
                | abs (f1+f2-fm*(fromInteger 2)) <= eps = integrate_iter_nosort func eps (right:others) ((area left right)+old_area)
                | otherwise = integrate_iter_nosort func eps (left:(Point xm fm):right:others) old_area
                where (Point xm fm)=midpoint func left right
    integrate_iter_nosort _ _ points old_area = (points, old_area)
        
    {-# INLINABLE integrate_nosort #-}
    integrate_nosort::(Fractional a, Ord a)=>(a->a)->[a]->a->a
    integrate_nosort func ticks eps=sum_area
            where 
                points=fmap (\x->Point x $ func x) ticks
                eps1=eps*(fromInteger 4)/((last ticks)-(head ticks))
                sum_area=snd $ integrate_iter_nosort func eps1 points $ fromInteger 0
                
            
