#include "adapt_trapezoid.hpp"

#include <queue>
#include <iostream>

using namespace std;

constexpr double PI=3.14159265358979323846;

double foo(double x){
    return std::sin(x*x);
}

int main()
{
    std::function<double(double)> foo1(foo);
    for(int i=0;i<100;++i){
    std::cout<<integrate(foo1,1e-10, std::vector<double>{0.0,1., 2., std::sqrt(8.0*PI)})<<std::endl;
    }
}
