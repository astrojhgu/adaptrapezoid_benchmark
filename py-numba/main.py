#!/usr/bin/env python
import timeit
import math

import numba as nb

import adapt


@nb.jit(nopython=True)
def foo(x):
    return math.sin(x * x)

#for i in range(0,100):
#    print(integrate(lambda x:math.sin(x**2), [0.0,1.0, 2.0, math.sqrt(8*math.pi)],1e-10))
    #print(integrate(lambda x: x ** 2, [0.0, 1.0], 1e-10))


print("Validating precision:")
TOL = 1e-10
PRECISE_RESULT = 0.527038339761566009286263102166809763899326865179511011538

print('integrate:')
result = adapt.integrate(foo, [0.0, 1.0, 2.0, math.sqrt(8 * math.pi)], TOL)
diff = abs(PRECISE_RESULT - result)
print("diff={0}".format(diff))
print("Required precision={0}".format(TOL))

n = 10

print('integrate:')
timer = timeit.Timer('integrate(foo, [0.0,1.0, 2.0, math.sqrt(8*math.pi)], 1e-10)', setup="from __main__ import foo; import math; from adapt import integrate;")

# Run twice, skipping the first which includes the jit cost
dt = timer.repeat(repeat=2, number=n)[-1]
print("dt={0} ms".format(dt / n * 1000))
