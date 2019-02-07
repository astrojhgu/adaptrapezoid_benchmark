import collections

import numba as nb
import numpy as np


Point = collections.namedtuple('Point', 'x y')


@nb.jit(nopython=True)
def sum_list(x):
    s = 0.0
    for k in range(len(x)):
        s += x[k]

    return s


@nb.jit(nopython=True)
def integrate_iter(func, eps, points):
    areas = []
    if len(points) < 2:
        return 0.0

    right = points[-1]
    sz = len(points)

    while sz > 1:
        left = points[-2]
        xm = 0.5 * (left.x + right.x)
        fm = func(xm)
        if abs(left.y + right.y - 2.0 * fm) <= eps:
            area = (left.y + right.y + fm * 2.0) * (right.x - left.x) * 0.25
            areas.append(area)
            points.pop()
            right = left
            sz -= 1
        else:
            points[-1] = Point(xm, fm)
            points.append(right)
            sz += 1

    areas_arr = np.array(areas)
    areas_arr = np.sort(areas_arr)
    return np.sum(areas_arr)

@nb.jit(nopython=True)
def integrate_iter_nosort(func, eps, points):
    total_area = 0.0
    if len(points) < 2:
        return 0.0

    right = points[-1]
    sz = len(points)

    while sz > 1:
        left = points[-2]
        xm = 0.5 * (left.x + right.x)
        fm = func(xm)
        if abs(left.y + right.y - 2.0 * fm) <= eps:
            area = (left.y + right.y + fm * 2.0) * (right.x - left.x) * 0.25
            total_area += area
            points.pop()
            right = left
            sz -= 1
        else:
            points[-1] = Point(xm, fm)
            points.append(right)
            sz += 1
    return total_area

@nb.jit(nopython=True)
def integrate(func, ticks, eps):
    eps1 = eps * 4.0 / (ticks[-1] - ticks[0])
    points = [Point(x, func(x))for x in ticks]
    return integrate_iter(func, eps1, points)


@nb.jit(nopython=True)
def integrate_nosort(func, ticks, eps):
    eps1 = eps * 4.0 / (ticks[-1] - ticks[0])
    points = [Point(x, func(x))for x in ticks]
    return integrate_iter_nosort(func, eps1, points)
