def neumaier_sum(x, sum, comp):
    t=sum+x
    if abs(sum)>=abs(x):
        comp+=(sum-t)+x
    else:
        comp+=(x-t)+sum
    sum=t
    return (sum, comp)



def integrate_iter(func, eps, points):
    total_area = 0.0
    comp = 0.0
    if len(points) < 2:
        return 0.0

    right = points[-1]
    sz = len(points)

    while sz > 1:
        left = points[-2]
        xm = 0.5 * (left[0] + right[0])
        fm = func(xm)
        if abs(left[1] + right[1] - 2.0 * fm) <= eps:
            area = (left[1] + right[1] + fm * 2.0) * (right[0] - left[0]) * 0.25
            total_area, comp = neumaier_sum(area, total_area, comp)

            points.pop()
            right = left
            sz -= 1
        else:
            points[-1] = (xm, fm)
            points.append(right)
            sz += 1
    return total_area+comp

def integrate(func, ticks, eps):
    eps1 = eps * 4.0 / (ticks[-1] - ticks[0])
    points = [(x, func(x)) for x in ticks]
    return integrate_iter(func, eps1, points)
