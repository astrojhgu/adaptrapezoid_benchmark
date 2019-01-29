#ifndef ADAPT_HPP
#define ADAPT_HPP

#include <algorithm>
#include <cmath>
#include <iterator>
#include <numeric>
#include <utility>
#include <vector>
#include <stack>
 
template <typename T, typename F>
T integrate(F f, T eps,  std::vector<T> ticks) {
  if (ticks.size() <= 1) {
    return T{};
  }
  constexpr T kHalf = T{0.5}, kQuarter = T{0.25};
  constexpr T kTwo = T{2}, kFour = T{4};
  eps = eps * kFour / (ticks.back() - ticks.front());

  std::vector<T> areas;

  std::stack<T> ss;

  auto right = ticks.back();
  auto right_f = f(right);
  ticks.pop_back();

  while (!ticks.empty()) {
    ss.push(f(ticks.back()));

    while (!ss.empty()) {
      auto left = ticks.back();
      auto left_f = ss.top(); 
      
      T mid = (left + right) * kHalf, fmid = f(mid);
      if (std::abs(left_f + right_f - kTwo * fmid) <= eps) {
        areas.push_back((left_f + right_f + fmid * kTwo) * (right - left) * kQuarter);
        right = left; right_f = left_f;
        ss.pop();
        ticks.pop_back();
      } else {
        ticks.push_back(mid);
        ss.push(fmid);
      }
    }
  }
  
  std::stable_sort(areas.begin(), areas.end(), [](auto x, auto y){return std::abs(x)<std::abs(y);});

  return std::accumulate(areas.begin(), areas.end(), static_cast<T>(0));
}


template <typename T, typename F>
T integrate_nosort(F f, T eps, std::vector<T> ticks) {
   if (ticks.size() <= 1) {
    return T{};
  }
  constexpr T kHalf = T{0.5}, kQuarter = T{0.25};
  constexpr T kTwo = T{2}, kFour = T{4};
  eps = eps * kFour / (ticks.back() - ticks.front());

  T result{};

  std::stack<T> ss;

  auto right = ticks.back();
  auto right_f = f(right);
  ticks.pop_back();

  while (!ticks.empty()) {
    ss.push(f(ticks.back()));

    while (!ss.empty()) {
      auto left = ticks.back();
      auto left_f = ss.top(); 
      
      T mid = (left + right) * kHalf, fmid = f(mid);
      if (std::abs(left_f + right_f - kTwo * fmid) <= eps) {
        result += (left_f + right_f + fmid * kTwo) * (right - left) * kQuarter;
        right = left; right_f = left_f;
        ss.pop();
        ticks.pop_back();
      } else {
        ticks.push_back(mid);
        ss.push(fmid);
      }
    }
  }
  return result;
}


#endif
