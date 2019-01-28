#include <algorithm>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <numeric>
#include <utility>
#include <vector>
#include <stack>
 
template <typename T>
struct Point {
  T x, f;
};
 
template <typename T, typename F>
T Integrate(F f, T eps, const std::vector<T>& init_ticks) {
  if (init_ticks.size() <= 1) {
    return T{};
  }
  constexpr T kHalf = T{0.5}, kQuarter = T{0.25};
  constexpr T kTwo = T{2}, kFour = T{4};
  eps = eps * kFour / (init_ticks.back() - init_ticks.front());

  T result{};
  std::vector<Point<T>> points;
  std::transform(begin(init_ticks), end(init_ticks), std::back_inserter(points),
                 [&f](T x) { return Point<T>{x, f(x)}; });

  std::stack<Point<T>, std::vector<Point<T>>> ss(std::move(points));

  auto right = ss.top();
  ss.pop();

  while (!ss.empty()) {
    auto const& left = ss.top(); 
    
    T mid = (left.x + right.x) * kHalf, fmid = f(mid);
    if (std::abs(left.f + right.f - kTwo * fmid) <= eps) {
      result += (left.f + right.f + fmid * kTwo) * (right.x - left.x) * kQuarter;
      right = left;
      ss.pop();
    } else {
      ss.push(Point<T>{mid, fmid});
    }
  }
  return result;
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
