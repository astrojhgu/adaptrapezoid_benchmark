use std::fmt::Debug;
use num_traits::float::Float;
use num_traits::float::FloatConst;

#[derive(Clone, Copy)]
struct Point<T>{
    x:T,
    f:T,
}

pub fn integrate<T>(func: &dyn Fn(T) -> T, eps: T, init_ticks: &[T]) -> T
where T: Float + Debug,
{
    if init_ticks.len()<=1{
        return T::zero();
    }
    let two=T::one()+T::one();
    let four=two+two;
    let half=T::one()/two;
    let quarter=half/two;
    let full_width=*init_ticks.last().unwrap()-*init_ticks.first().unwrap();
    let mut areas=Vec::<T>::new();
    let mut points:Vec<_>=init_ticks.iter().map(|&x|Point::<T>{x, f:func(x)}).collect();
    let mut right=*points.last().unwrap();
    let mut sz=points.len();
    let eps=eps*four/full_width;
    while sz>1 {
        let left=points[sz-2];
        let mid=(left.x+right.x)*half;
        let fmid=func(mid);
        if (left.f+right.f-fmid*two).abs()<=eps{
            areas.push((left.f+right.f+fmid*two)*(right.x-left.x)*quarter);
            points.pop();
            right=left;
            sz-=1;
        }
        else{
            points.pop();
            points.push(Point::<T>{x:mid, f:fmid});
            points.push(right);
            sz+=1;
        }
    }
    (&mut areas).sort_by(|a, b| {
        if a.abs() < b.abs() {
            std::cmp::Ordering::Less
        } else if a.abs() > b.abs() {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        }});
    areas.into_iter().fold(T::zero(), |a, b| a + b)
}

fn main(){
    /*
    for _i in 0..100 {
        println!(
            "{}",
            integrate(
                &|x: f64| x.powi(2),
                1e-10,
                &[0.0, 1.0]
            )
        );
    }
    */

        for _i in 0..100 {
        println!(
            "{}",
            integrate(
                &|x: f64| x.powi(2).sin(),
                1e-10,
                &[0.0, 1.0, 2.0, (8.0*f64::PI()).sqrt()]
            )
        );
    }

}