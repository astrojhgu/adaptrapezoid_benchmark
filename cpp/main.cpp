#include "adapt.hpp"
#include <cmath>
#include <iomanip>
#include <iostream>
#include <utility>
#include <vector>

#include <benchmark/benchmark.h>
constexpr double PI = 3.14159265358979323846;
constexpr double tol=1e-10;

double foo (double x)
{
    return std::sin (x * x);
}

void run (benchmark::State &state)
{
    // std::cout<<"Result 1="<<std::setprecision(10)<<integrate(foo, 1e-10,
    // std::vector<double>{0.0, 1.0, 2.0, std::sqrt(8.0 * PI)})<<std::endl;
    for (auto _ : state)
        {
            integrate (foo, tol, std::vector<double>{ 0.0, 1.0, 2.0, std::sqrt (8.0 * PI) });
        }
}

void run_nosort (benchmark::State &state)
{
    // std::cout<<"Result 2="<<std::setprecision(10)<<integrate_nosort(foo, tol,
    // std::vector<double>{0.0, 1.0, 2.0, std::sqrt(8.0 * PI)})<<std::endl;

    for (auto _ : state)
        {
            integrate_nosort (foo, tol, std::vector<double>{ 0.0, 1.0, 2.0, std::sqrt (8.0 * PI) });
        }
}


BENCHMARK (run);
BENCHMARK (run_nosort);

int main (int argc, char* argv[]) {
    constexpr double precise_answer=0.527038339761566009286263102166809763899326865179511011538;
    assert(std::abs(precise_answer-integrate_nosort (foo, tol, std::vector<double>{ 0.0, 1.0, 2.0, std::sqrt (8.0 * PI) }))<tol);
    assert(precise_answer-integrate (foo, tol, std::vector<double>{ 0.0, 1.0, 2.0, std::sqrt (8.0 * PI) })<tol);
 
    benchmark::Initialize (& argc, argv);
    benchmark::RunSpecifiedBenchmarks ();
    return 0;
}
