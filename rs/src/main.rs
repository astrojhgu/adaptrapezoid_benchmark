use num_traits::float::Float;
use num_traits::float::FloatConst;
use std::collections::linked_list::LinkedList;
use std::fmt::Debug;

struct Interval<T>
where
    T: Float,
{
    x1: T,
    f1: T,
    x2: T,
    f2: T,
    subsum: T,
}

enum RefineResult<T>
where
    T: Float,
{
    NoMoreRefine(Interval<T>, Interval<T>),
    Refined(Interval<T>, Interval<T>),
}

impl<T> Interval<T>
where
    T: Float,
{
    pub fn split(self, func:&dyn Fn(T)->T)->(Self, Self){
        let one = T::one();
        let two = one + one;
        let xm = (self.x1 + self.x2) / two;
        let fm = func(xm);
        let ss1 = (self.f1 + fm) * (xm - self.x1) / two;
        let ss2 = (fm + self.f2) * (self.x2 - xm) / two;
        (
            Interval {
                x1: self.x1,
                f1: self.f1,
                x2: xm,
                f2: fm,
                subsum: ss1,
            },
            Interval {
                x1: xm,
                f1: fm,
                x2: self.x2,
                f2: self.f2,
                subsum: ss2,
            },
        )
    }

    pub fn try_refine(self, func: &dyn Fn(T) -> T, eps: T) -> RefineResult<T> {
        let ss = self.subsum;
        let (i1, i2) = self.split(func);

        if (i1.subsum + i2.subsum - ss).abs() <= eps {
            RefineResult::NoMoreRefine(i1, i2)
        } else {
            RefineResult::Refined(i1, i2)
        }
    }
}


fn refine_iter<T>(
    func: &dyn Fn(T) -> T,
    (not_refined, mut refined_result): (LinkedList<Interval<T>>, LinkedList<Interval<T>>),
    eps: T,
    original_width: T,
) -> (LinkedList<Interval<T>>, LinkedList<Interval<T>>)
where
    T: Float,
{
    let mut updated_not_refined = LinkedList::new();
    for i in not_refined.into_iter() {
        let interval_width = i.x2 - i.x1;
        match i.try_refine(func, eps / original_width * interval_width) {
            RefineResult::NoMoreRefine(i1, i2) => {
                refined_result.push_back(i1);
                refined_result.push_back(i2);
            }
            RefineResult::Refined(i1, i2) => {
                updated_not_refined.push_back(i1);
                updated_not_refined.push_back(i2);
            }
        }
    }
    (updated_not_refined, refined_result)
}

fn refine_until_converged<T>(
    func: &dyn Fn(T) -> T,
    init_list: LinkedList<Interval<T>>,
    eps: T,
) -> LinkedList<Interval<T>>
where
    T: Float,
{
    let original_width = init_list.back().unwrap().x2 - init_list.front().unwrap().x1;
    let mut unrefined_list = init_list;
    let mut refined_result = LinkedList::new();
    loop {
        let refined = refine_iter(func, (unrefined_list, refined_result), eps, original_width);
        unrefined_list = refined.0;
        refined_result = refined.1;
        if unrefined_list.is_empty() {
            break refined_result;
        }
    }
}

fn sum_up<T>(il: LinkedList<Interval<T>>) -> T
    where
        T: Float + Debug,
{
    let mut sorted: Vec<_> = il.into_iter().map(|x| x.subsum).collect();

    (&mut sorted).sort_by(|a, b| {
        if a.abs() < b.abs() {
            std::cmp::Ordering::Less
        } else if a.abs() > b.abs() {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        }
    });

    sorted.into_iter().fold(T::zero(), |a, b| a + b)
}


pub fn integrate<T>(func: &dyn Fn(T) -> T, eps: T, init_ticks: &[T]) -> T
where
    T: Float + Debug,
{
    let init_list: LinkedList<_> = init_ticks[1..]
        .iter()
        .zip(init_ticks[0..].iter())
        .map(|(&x2, &x1)| {
            let f1 = func(x1);
            let f2 = func(x2);
            let subsum = (f2 + f1) * (x2 - x1) / (T::one() + T::one());
            Interval::<T> {
                x1,
                f1,
                x2,
                f2,
                subsum,
            }
        }).collect();
    sum_up(refine_until_converged(func, init_list, eps))
}

fn main() {
    for _i in 0..100 {
        println!(
            "{}",
            integrate(
                &|x: f64| x.powi(2).sin(),
                1e-10,
                &[0.0, 1.0, 2.0, (8.0 * f64::PI()).sqrt()]
            )
        );
    }
}
