import collections

import numba as nb
import numpy as np


Point = collections.namedtuple('Point', 'x y')

@nb.jit(nopython=True)
def neumaier_sum(x, sum, comp):
    t=sum+x
    if abs(sum)>=abs(x):
        comp+=(sum-t)+x
    else:
        comp+=(x-t)+sum
    sum=t
    return (sum, comp)


@nb.jit(nopython=True)
def integrate_iter(func, eps, points):
    total_area = 0.0
    comp=0.0
    if len(points) < 2:
        return 0.0

    right = points.pop()

    while points:
        left = points[-1]
        xm = 0.5 * (left.x + right.x)
        fm = func(xm)
        if abs(left.y + right.y - 2.0 * fm) <= eps:
            area = (left.y + right.y + fm * 2.0) * (right.x - left.x) * 0.25
            total_area, comp=neumaier_sum(area, total_area, comp)
            right=points.pop()
        else:
            points.append(Point(xm, fm))
    return total_area+comp

@nb.jit(nopython=True)
def integrate(func, ticks, eps):
    eps1 = eps * 4.0 / (ticks[-1] - ticks[0])
    points = [Point(x, func(x))for x in ticks]
    return integrate_iter(func, eps1, points)
