#!/usr/bin/env python

class Interval:
    def __init__(self, x1, f1, x2, f2):
        self.x1=x1
        self.f1=f1
        self.x2=x2
        self.f2=f2
        self.subsum=(f1+f2)*(x2-x1)/2.0
        pass

    def split(self, func):
        xm=(self.x1+self.x2)/2
        fm=func(xm)
        return (Interval(self.x1, self.f1, xm, fm), Interval(xm, fm, self.x2, self.f2))

    def try_refine(self, func, eps):
        (i1,i2)=self.split(func)
        return (i1,i2,abs(i1.subsum+i2.subsum-self.subsum)<eps)

    def interval_width(self):
        return self.x2-self.x1


def refine_iter(func, unrefined, refined, eps, full_width):
    new_unrefined=[]
    for i in unrefined:
        (i1,i2, finished)=i.try_refine(func, eps/full_width*i.interval_width())
        if finished:
            refined.append(i1)
            refined.append(i2)
        else:
            new_unrefined.append(i1)
            new_unrefined.append(i2)
    return (new_unrefined, refined)


def refine_until_converged(func, initial_intervals, eps, full_width):
    refined=[]
    unrefined=initial_intervals
    while unrefined:
        unrefined, refined=refine_iter(func, unrefined, refined, eps, full_width)
    return refined

def init_intervals(func, ticks):
    return [Interval(x1,func(x1),x2,func(x2)) for (x1,x2) in zip(ticks[0::], ticks[1::])]

def integrate(func, ticks, eps):
    full_width=ticks[-1]-ticks[0]
    refined=refine_until_converged(func, init_intervals(func, ticks), eps, full_width)

    subsums=[i.subsum for i in refined]
    result=0.0
    subsums.sort(key=abs)
    for i in subsums:
        result+=i
    return result

import math
print(integrate(lambda x:math.sin(x**2), [0.0,1.0, 2.0, math.sqrt(8*math.pi)],1e-10))
