#!/usr/bin/env python

def integrate_iter(func, eps, points):
    areas=[]
    if len(points)<2:
        return 0.0
    right=points[-1]
    sz=len(points)
    while sz>1 :
        left=points[-2]
        xm=(left[0]+right[0])/2.
        fm=func(xm)
        if abs(left[1]+right[1]-2.0*fm)<=eps:
            area=(left[1]+right[1]+fm*2.0)*(right[0]-left[0])/4.0
            areas.append(area)
            points.pop()
            right=left
            sz-=1
        else:
            points[-1]=(xm, fm)
            points.append(right)
            sz+=1
    areas.sort(key=abs)
    return sum(areas)


def integrate(func, ticks, eps):
    eps1=eps*4.0/(ticks[-1]-ticks[0])
    points=[(x, func(x))for x in ticks]
    return integrate_iter(func, eps1, points)


for i in range(0,100):
    #print(integrate(lambda x:math.sin(x**2), [0.0,1.0, 2.0, math.sqrt(8*math.pi)],1e-10))
    print(integrate(lambda x: x ** 2, [0.0, 1.0], 1e-10))
