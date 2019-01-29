#include "adapt.hpp"
#include <cmath>
#include <iomanip>
#include <iostream>
#include <utility>
#include <vector>

#include <benchmark/benchmark.h>


double foo(double x) { return std::sin(x * x); }

void run(benchmark::State& state) {
  constexpr double PI = 3.14159265358979323846;
  //std::cout<<"Result 1="<<std::setprecision(10)<<integrate(foo, 1e-10, std::vector<double>{0.0, 1.0, 2.0, std::sqrt(8.0 * PI)})<<std::endl;
  for(auto _: state){
    integrate(foo, 1e-10, std::vector<double>{0.0, 1.0, 2.0, std::sqrt(8.0 * PI)});
  }
}

void run_nosort(benchmark::State& state) {
  constexpr double PI = 3.14159265358979323846;
  //std::cout<<"Result 2="<<std::setprecision(10)<<integrate_nosort(foo, 1e-10, std::vector<double>{0.0, 1.0, 2.0, std::sqrt(8.0 * PI)})<<std::endl;

  for(auto _: state){
    integrate_nosort(foo, 1e-10, std::vector<double>{0.0, 1.0, 2.0, std::sqrt(8.0 * PI)});
  }
}


BENCHMARK(run);
BENCHMARK(run_nosort);
BENCHMARK(run_more);
BENCHMARK_MAIN();
