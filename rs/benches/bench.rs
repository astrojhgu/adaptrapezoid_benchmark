#[macro_use]
extern crate criterion;

use criterion::Criterion;

use adaptrapezoid::integrate;
use num_traits::float::FloatConst;

fn run(c: &mut Criterion) {
    c.bench_function("with sorting", move |b| {
        b.iter(|| {
            integrate(
                &|x: f64| x.powi(2).sin(),
                1e-10,
                &[0.0, 1.0, 2.0, (8.0 * f64::PI()).sqrt()],
            )
        })
    });
}

criterion_group!(name=benches; config=Criterion::default().sample_size(30).nresamples(10); targets=run);
criterion_main!(benches);
