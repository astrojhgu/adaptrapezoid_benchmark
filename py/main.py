#!/usr/bin/env python
import math
import adapt

#for i in range(0,100):
#    print(integrate(lambda x:math.sin(x**2), [0.0,1.0, 2.0, math.sqrt(8*math.pi)],1e-10))
    #print(integrate(lambda x: x ** 2, [0.0, 1.0], 1e-10))
import timeit

n=10
timer=timeit.Timer('integrate(lambda x:math.sin(x**2), [0.0,1.0, 2.0, math.sqrt(8*math.pi)],1e-10)', setup="import math; from adapt import integrate")
dt=timer.timeit(n)
print("dt={0} ms".format(dt/n*1000))
