# A benchmark of different languages that are potentially suitable to scientific computing with the adaptive trapezoid integration algorithm.

Currently implemented languages:
Rust, C++, Scala, Haskell, Python, C#, C

# Directories
1. rs: Rust
2. cpp: C++
3. hs: Haskell
4. py: Python
5. scala: Scala
6. cs: C#
7. c: C

# Benchmark results
The algorithm is imlemented in two ways:
1. After computing the sub-results of each interval, sort them before summing up: [Result_sort.md](Result_sort.md).
2. Directly sum up the results of each interval without sorting: [Result_nosort.md](Result_nosort.md).

Behcnmark methods for each language are listed in [Benchcmd.md](Benchcmd.md).

# Description to the algorithm
The adaptive trapezoid quadrature method (i.e., the definite integration) works by dividing the integration interval iteratively (or in other words, recursively) and approximate the result by the summing areas of trapezoids of all the intervals.

The detailed algorithm is
1. Setting up the function F to be integrated, setting the tolerant value ```eps```.
2. Initialize a set (more than 1) of initial ticks, which defines the initial intervals. The x values of the initial ticks should be in increasing order. Say a set of points with X's=x_i (i=0,1,2,..n-1), then the corresponding initial intervals are
```[x_0, x_1], [x_1, x_2], [x_2, x_3], ...[x_{n-2}, x_{n-1}]```
3. The result of any interval ```[x_1, x_2]``` is calculated as ```I(F, x_1, x_2)```, which is defined in following algorithm.
4. Sorting the result of each interval according to their ```abs``` in increasing order, and add them up.

The definition of ```I(F, x_1, x_2)``` is
1. Calculate the ```diff=T(F, x_1, x_2)-T(F, x_1, (x_1+x_2)/2)-T(F, (x_1+x_2)/2, x_2)```, where ```T(F, a, b)=(F(a)+F(b))*(b-a)/2```.
2. If ```diff<eps/W*(x_2-x_1)```, go to 3, otherwise go to 4 where ```W``` is the width of the whole initial integration interval.
3. return ```T(F, x_1, (x_1+x_2)/2)+T(F, (x_1+x_2)/2, x_2)```.
4. return ```I(F, x_1, (x_1+x_2)/2)+I(F, (x_1+x_2)/2, x_2)```.

When summing up the sub-results of each interval, sorting the sub-results by their ```abs``` are optionally performed in order to suppress the floating number truncation effect during adding a small number to a large one.
