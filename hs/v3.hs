module Main where
    import Data.List

    -- represent an interval
    -- a1 a2 are the lower and upper boundary of this interval
    -- f_i=func a_i, where i=1,2 respectively
    data Interval a=Interval a a a a deriving Show -- a1 f1 a2 f2 subsum 

    -- calculate the trapezoid area defined by the interval
    subsum::(Fractional a)=>(Interval a)->a
    subsum (Interval a1 f1 a2 f2)=(f1+f2)*(a2-a1)/(fromInteger 2)

    -- split an interval into two sub-intervals based on the function and the
    -- original interval
    split::(Fractional a)=>(a->a)->(Interval a)->((Interval a), (Interval a))
    split func (Interval a1 f1 a2 f2)=((Interval a1 f1 am fm), 
                                    (Interval am fm a2 f2))
                                    where
                                        am=(a1+a2)/ (fromInteger 2)
                                        fm=func am
    
    -- Refine result of an interval
    -- if the area of one trapezoid differs from the 
    -- sum of the splitted trapezoids no more than eps
    -- then this trapezoid needs no more refinement
    -- other more refinement is required.
    -- no matter whether more refinement is required, 
    -- the splitted intervals are conserved.
    data RefineResult a=NoMoreRefine (Interval a) (Interval a)
                        |NeedMoreRefine (Interval a) (Interval a)

    -- try to refine an interval
    -- return NoMoreRefine or NeedMoreRefine depending on
    -- whether more refinement is required
    try_refine::(Fractional a, Ord a)=>
        (a->a)->(Interval a)->a->a->(RefineResult a)
    try_refine func origin@(Interval a1 f1 a2 f2) eps full_width
            | (abs ((subsum i1)+(subsum i2)-(subsum origin))) 
                > (eps*(a2-a1)/full_width) =NeedMoreRefine i1 i2
            | otherwise = NoMoreRefine i1 i2
            where (i1, i2)=split func origin

    -- perform the interval refinement over a list of initial intervals
    -- Besides the function to be integrated, the eps that defines the precision
    -- a pair of [Interval a] is given.
    -- The first [Interval a] defines the list of intervals to be refined
    -- and the second [Interval a] defines the list of intervals that has been
    -- refined (hense no more refinement needed)
    -- This function runs recursively and returns when no interval is left to be
    -- further refined.
    refine_iter::(Fractional a, Ord a)=>
        (a->a)->([Interval a], [Interval a])->a->a->([Interval a], [Interval a])
    refine_iter func ([], refined) _ _ =([], refined)
    refine_iter func (i:others, refined) eps full_width=
        refine_iter func (disp (try_refine func i eps full_width) 
                others refined) eps full_width
            where
                disp (NoMoreRefine i1 i2) others refined=
                    (others, i1:i2:refined)
                disp (NeedMoreRefine i1 i2) others refined=
                    (i1:i2:others, refined)

    -- initialize a list of intervals from a list of initial ticks
    init_interval::(Fractional a)=>(a->a)->[a]->[Interval a]
    init_interval func ticks=map (pair2Interval func ) (zip (init ticks) (tail ticks))
                            where
                                pair2Interval func (a1,a2)=Interval a1 (func a1) a2 (func a2)

    -- after refinement finished, sum up all subsum's
    sum_up::(Fractional a, Ord a)=>[Interval a]->a
    sum_up interval_list = foldr (+) (fromInteger 0) $fmap subsum interval_list

    -- perform the integration
    -- a function f, a list of initial ticks and a required precision eps is given
    integrate::(Fractional a, Ord a)=>(a->a)->[a]->a->a
    integrate func ticks eps=sum_up 
            $ sortBy (\a b->compare (abs $ subsum a) (abs $ subsum b)) 
            $ snd $refine_iter func ((init_interval func ticks), []) eps full_width
                            where full_width=(last ticks)-(head ticks)

    -- test function to be integrated
    foo x= sin (x*x)
    

    main :: IO ()
    main = print $ integrate foo [0.0, 1.0, 2.0, sqrt (8*pi)] 1e-10
    