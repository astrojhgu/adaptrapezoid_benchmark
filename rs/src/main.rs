#![feature(test)]

extern crate test;
//extern crate adaptrapezoid;

use adaptrapezoid::integrate;
use adaptrapezoid::integrate_nosort;
use test::Bencher;
use num_traits::float::FloatConst;


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


#[bench]
fn run(b:&mut Bencher){
    b.iter(||{
        integrate(
            &|x: f64| x.powi(2).sin(),
            1e-10,
            &[0.0, 1.0, 2.0, (8.0*f64::PI()).sqrt()]
        )
    })
}

#[bench]
fn run_nosort(b:&mut Bencher){
    b.iter(||{
        integrate_nosort(
            &|x: f64| x.powi(2).sin(),
            1e-10,
            &[0.0, 1.0, 2.0, (8.0*f64::PI()).sqrt()]
        )
    })
}
