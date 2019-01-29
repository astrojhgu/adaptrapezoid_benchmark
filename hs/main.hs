module Main where
    import Data.List

    data Point a=Point a a deriving Show -- x f

    midpoint::(Fractional a)=>(a->a)->(Point a)->(Point a)->(Point a)
    midpoint func (Point x1 f1) (Point x2 f2)=Point xm fm
        where 
            xm=(x1+x2)/(fromInteger 2)
            fm=func xm

    area::(Fractional a)=>(Point a)->(Point a)->a
    area (Point x1 f1) (Point x2 f2)=(f1+f2)*(x2-x1)/(fromInteger 2)
    

    integrate_iter::(Fractional a, Ord a)=>(a->a)->a->[Point a]->[a]->([Point a], [a])
    integrate_iter func eps (left@(Point x1 f1):right@(Point x2 f2):others) areas
                | abs (f1+f2-fm*(fromInteger 2)) <= eps = integrate_iter func eps (right:others) ((area left right):areas)
                | otherwise = integrate_iter func eps (left:(Point xm fm):right:others) areas
                where (Point xm fm)=midpoint func left right
    integrate_iter _ _ points areas = (points, areas)
        
    integrate::(Fractional a, Ord a)=>(a->a)->[a]->a->a
    integrate func ticks eps=foldr (+) (fromInteger 0) sorted_areas
            where 
                points=fmap (\x->Point x $ func x) ticks
                eps1=eps*(fromInteger 4)/((last ticks)-(head ticks))
                areas=snd $ integrate_iter func eps1 points []
                sorted_areas=sortBy (\a b->compare (abs a) (abs b)) areas

    -- test function to be integrated
    foo x= sin (x*x)
    

    main :: IO ()
    main = print $ integrate foo [0.0, 1.0, 2.0, sqrt (8*pi)] 1e-10
    