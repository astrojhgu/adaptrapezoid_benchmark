use std::fmt::Debug;

use num_traits::float::Float;
use num_traits::float::FloatConst;

#[derive(Clone, Copy)]
struct Point<T> {
    x: T,
    f: T,
}

fn neumaier_sum<T>(x:T, mut sum:T, mut comp:T)->(T, T)
where T:Float+Debug
{
    //https://en.wikipedia.org/wiki/Kahan_summation_algorithm
    let t=sum+x;
    if sum.abs()>=x.abs(){
        comp=comp+((sum-t)+x);
    }else{
        comp=comp+((x-t)+sum);
    }
    sum=t;
    (sum, comp)
}


pub fn integrate<T>(func: &dyn Fn(T) -> T, eps: T, init_ticks: &[T]) -> T
where
    T: Float + Debug,
{
    if init_ticks.len() <= 1 {
        return T::zero();
    }
    let two = T::one() + T::one();
    let four = two + two;
    let half = T::one() / two;
    let quarter = half / two;
    let full_width = *init_ticks.last().unwrap() - *init_ticks.first().unwrap();
    let mut total_area = T::zero();
    let mut comp = T::zero();
    let mut points: Vec<_> = init_ticks
        .iter()
        .map(|&x| Point::<T> { x, f: func(x) })
        .collect();
    let mut right=points.pop().unwrap();
    let eps = eps * four / full_width;
    while !points.is_empty() {
        let left = *points.last().unwrap();
        let mid = (left.x + right.x) * half;
        let fmid = func(mid);
        if (left.f + right.f - fmid * two).abs() <= eps {
            let area = (left.f + right.f + fmid * two) * (right.x - left.x) * 
            quarter;
            let (s, c)=neumaier_sum(area, total_area, comp);
            total_area=s;
            comp=c;
            right=left;
            points.pop();
        } else {
            points.push(Point::<T> { x: mid, f: fmid });
        }
    }
    total_area+comp
}
