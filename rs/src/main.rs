use adaptrapezoid::integrate;
use adaptrapezoid::integrate_nosort;
use num_traits::float::FloatConst;
const PRECISE_RESULT: f64 = 0.527038339761566009286263102166809763899326865179511011538;
const TOL: f64 = 1e-10;
fn main() {
    println!("Validating precision");
    println!("The precise result should be: {}", PRECISE_RESULT);
    let result = integrate(
        &|x: f64| x.powi(2).sin(),
        TOL,
        &[0.0, 1.0, 2.0, (8.0 * f64::PI()).sqrt()],
    );
    println!("The required precision is {:e}", TOL);
    println!("The actual result of integrate is {}", result);
    println!("diff= {:e}", (result - PRECISE_RESULT).abs());
    let result = integrate_nosort(
        &|x: f64| x.powi(2).sin(),
        TOL,
        &[0.0, 1.0, 2.0, (8.0 * f64::PI()).sqrt()],
    );
    println!("The actual result of integrate_nosort is {}", result);
    println!("diff= {:e}", (result - PRECISE_RESULT).abs());
}
