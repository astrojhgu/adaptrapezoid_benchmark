#include <algorithm>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <numeric>
#include <utility>
#include <vector>

template <typename T>
struct Point {
  T x, f;
};
 
template <typename T, typename F>
T Integrate(F f, T eps, const std::vector<T>& init_ticks) {
  if (init_ticks.size() <= 1) {
    return static_cast<T>(0);
  }
  constexpr T kHalf = static_cast<T>(0.5), kQuarter = static_cast<T>(0.25);
  constexpr T kTwo = static_cast<T>(2), kFour = static_cast<T>(4);
  eps = eps * kFour / (init_ticks.back() - init_ticks.front());
  std::vector<T> areas;
  std::vector<Point<T>> points;
  points.reserve(init_ticks.size());
  std::transform(init_ticks.begin(), init_ticks.end(), std::back_inserter(points),
                 [&f](const T& x) {
                   return Point<T>{x, f(x)};
                 });
  Point<T> right;
  for (size_t sz = points.size(); sz > 1;) {
    right=points.back();
    points.pop_back();
    auto& left = points.back();
    T mid = (left.x + right.x) * kHalf, fmid = f(mid);
    if (std::abs(left.f + right.f - kTwo * fmid) <= eps) {
      areas.push_back((left.f + right.f + fmid * kTwo) * (right.x - left.x) * kQuarter);
      //points.pop_back();
      //right = left;
      --sz;
    } else {
      points.push_back(Point<T>{mid, fmid});
      points.push_back(right);
      ++sz;
    }
  }
  std::stable_sort(areas.begin(), areas.end(), [](auto x, auto y){return std::abs(x)<std::abs(y);});
  //std::sort(areas.begin(), areas.end());
  return std::accumulate(areas.begin(), areas.end(), static_cast<T>(0));
}
 
double foo(double x) { return std::sin(x * x); }
 
int main() {
  constexpr double PI = 3.14159265358979323846;
  for (int i = 0; i < 100; ++i) {
    std::cout << std::setprecision(16)
              << Integrate(foo, 1e-10, std::vector<double>{0.0, 1.0, 2.0, std::sqrt(8.0 * PI)})
              << std::endl;
  }
}
