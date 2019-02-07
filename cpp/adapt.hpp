#ifndef ADAPT_HPP
#define ADAPT_HPP
#include <tuple>
#include <algorithm>
#include <cmath>
#include <iterator>
#include <numeric>
#include <utility>
#include <vector>
#include <stack>

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



template <typename T, typename F> T integrate(F f, T eps, std::vector<T> ticks)
{
    if (ticks.size () <= 1)
        {
            return T{};
        }
    constexpr T kHalf = T{ 0.5 }, kQuarter = T{ 0.25 };
    constexpr T kTwo = T{ 2 }, kFour = T{ 4 };
    eps = eps * kFour / (ticks.back () - ticks.front ());

    T result{};
    T comp{};
    std::stack<T> ss;

    auto right = ticks.back ();
    auto right_f = f (right);
    ticks.pop_back ();

    while (!ticks.empty ())
        {
            ss.push (f (ticks.back ()));

            while (!ss.empty ())
                {
                    auto left = ticks.back ();
                    auto left_f = ss.top ();

                    T mid = (left + right) * kHalf, fmid = f (mid);
                    if (std::abs (left_f + right_f - kTwo * fmid) <= eps)
                        {
                            auto area = (left_f + right_f + fmid * kTwo) * (right - left) * kQuarter;
                            right = left;
                            right_f = left_f;
                            ss.pop ();
                            ticks.pop_back ();
                            std::tie(result, comp)=neumaier_sum(area, result, comp);
                        }
                    else
                        {
                            ticks.push_back (mid);
                            ss.push (fmid);
                        }
                }
        }
    return result;
}


#endif
