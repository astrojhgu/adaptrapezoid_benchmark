#ifndef ADAPT_HPP
#define ADAPT_HPP
#include <tuple>
#include <algorithm>
#include <iostream>
#include <cmath>
#include <iterator>
#include <numeric>
#include <utility>
#include <vector>
#include <stack>

template <typename T>
struct Point{
    T x;
    T f;

    Point(T x1, T f1)
    :x(x1), f(f1){
    }

    Point()=default;
    Point(const Point&)=default;
    Point& operator=(const Point&)=default;
    ~Point()=default;
};

template <typename T, typename F>
inline Point<T> midpoint (F f, const Point<T>& p1, const Point<T>& p2)
{
    double xm = (p1.x + p2.x) / 2.0;
    Point<T> p(xm, f(xm));
    return p;
}


template <typename T>
inline std::tuple<T, T> neumaier_sum(T x, T sum, T comp)
{
    //https://en.wikipedia.org/wiki/Kahan_summation_algorithm
    T t=sum+x;
    if (std::abs(sum)>=std::abs(x)){
        comp+=((sum-t)+x);
    }else{
        comp+=((x-t)+ sum);
    }
    sum=t;
    return std::make_tuple(sum, comp);
}



template <typename T, typename F> T integrate(F func, T eps, std::vector<T> ticks)
{
    if (ticks.size () <= 1)
        {
            return T{};
        }
    constexpr T kHalf = T{ 0.5 }, kQuarter = T{ 0.25 };
    constexpr T kTwo = T{ 2 }, kFour = T{ 4 };
    
    eps = eps * kFour / (ticks.back () - ticks.front ());
    
    T total_area{};
    T comp{};
    std::stack<Point<T>, std::vector<Point<T>>> ps;
    for(auto x:ticks){
        ps.push(Point<T>(x, func(x)));
    }
    Point<T> right=ps.top();
    ps.pop();
    size_t loop_cnt=0;
    while (!ps.empty())
        {
            loop_cnt+=1;
            Point<T> left = ps.top();
            Point<T> midp = midpoint (func, left, right);
            if (std::abs (left.f + right.f - midp.f * kTwo) <= eps)
                {
                    T s = (left.f + right.f + midp.f * 2.0) * (right.x - left.x) / 4.0;
                    std::tie(total_area, comp)=neumaier_sum(s, total_area, comp);
                    right = ps.top();
                    ps.pop();
                }
            else
                {
                    ps.push(midp);
                }
        }
    std::cout<<loop_cnt<<std::endl;
    return total_area+comp;
}

#endif
