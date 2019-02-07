use std::fmt::Debug;

use num_traits::float::Float;
use num_traits::float::FloatConst;

#[derive(Clone, Copy)]
struct Point<T> {
    x: T,
    f: T,
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
    //let mut right=*points.last().unwrap();
    let mut right;

    let eps = eps * four / full_width;
    while points.len() > 1 {
        right = points.pop().unwrap();
        let left = *points.last().unwrap();
        let mid = (left.x + right.x) * half;
        let fmid = func(mid);
        if (left.f + right.f - fmid * two).abs() <= eps {
            let area = (left.f + right.f + fmid * two) * (right.x - left.x) * 
            quarter - comp;
            let t = total_area + area;
            comp = (t-total_area)-area;
            total_area = t;
        //points.pop();
        //right=left;
        } else {
            //points.pop();
            points.push(Point::<T> { x: mid, f: fmid });
            points.push(right);
        }
    }
    total_area
}
