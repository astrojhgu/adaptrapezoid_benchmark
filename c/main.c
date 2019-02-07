#include <math.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#define STACK_CAP 1024

typedef struct
{
    double x;
    double f;
} Point;

inline Point midpoint (double (*func) (double), Point p1, Point p2)
{
    double xm = (p1.x + p2.x) / 2.0;
    Point p = { .x = xm, .f = func (xm) };
    return p;
}
///////////////stack/////////////////
typedef struct _PointStack
{
    Point *mem;
    int cap;
    int top_idx; // point to the current top
} PointStack;

inline void extend_stack (PointStack *ps, int new_cap)
{
    ps->mem = (Point *)realloc (ps->mem, new_cap * sizeof (Point));
    assert (ps->mem != 0);
    ps->cap = new_cap;
    // printf("extended\n");
}

inline void push (PointStack *ps, Point p)
{
    if (ps->top_idx >= ps->cap - 1)
        {
            extend_stack (ps, ps->cap * 2);
        }
    ++ps->top_idx;
    ps->mem[ps->top_idx] = p;
}

inline Point pop (PointStack *ps)
{
    return ps->mem[ps->top_idx--];
}

inline Point top (PointStack *ps)
{
    return ps->mem[ps->top_idx];
}

inline int stack_size (PointStack *ps)
{
    return ps->top_idx + 1;
}

inline PointStack init_point_stack (int cap)
{
    Point *mem = (Point *)malloc (sizeof (Point) * cap);
    PointStack pp = { mem, cap, -1 };
    return pp;
}

inline void finalize_point_stack (PointStack *ps)
{
    free (ps->mem);
    ps->mem = 0;
    ps->cap = 0;
    ps->top_idx = -1;
}

inline void neumaier_sum(double x, double* sum, double* comp)
{
    //https://en.wikipedia.org/wiki/Kahan_summation_algorithm
    double t=*sum+x;
    if (fabs(*sum)>=fabs(x)){
        *comp+=((*sum-t)+x);
    }else{
        *comp+=((x-t)+ *sum);
    }
    *sum=t;
}



double integrate (double (*func) (double), double *init_ticks, int nticks, double eps1)
{
    if (nticks <= 1)
        {
            return 0;
        }
    double full_width = init_ticks[nticks - 1] - init_ticks[0];
    double area=0;
    double comp=0;
    PointStack ps = init_point_stack (STACK_CAP);
    for (int i = 0; i < nticks; ++i)
        {
            Point p = { .x = init_ticks[i], .f = func (init_ticks[i]) };
            push (&ps, p);
        }
    Point right;
    double eps = eps1 * 4.0 / full_width;
    while (stack_size (&ps) > 1)
        {
            Point right = pop (&ps);
            Point left = top (&ps);
            Point midp = midpoint (func, left, right);
            if (fabs (left.f + right.f - midp.f * 2) <= eps)
                {
                    double s = (left.f + right.f + midp.f * 2.0) * (right.x - left.x) / 4.0;
                    neumaier_sum(s, &area, &comp);
                }
            else
                {
                    push (&ps, midp);
                    push (&ps, right);
                }
        }
    finalize_point_stack (&ps);
    return area;
}

inline double foo (double x)
{
    return sin (x * x);
}


int main (int argc, char *argv[])
{
    const double tol=1e-10;
    const double PI = 3.14159265358979323846;
    const double precise_answer=0.527038339761566009286263102166809763899326865179511011538;
    double init_ticks[] = { 0.0, 1.0, 2.0, sqrt (8. * PI) };
    for (int i = 0; i < 100; ++i)
        {
            double result=integrate (foo, init_ticks, sizeof (init_ticks) / sizeof (double), tol);
            assert(fabs(result-precise_answer)<tol);
            printf ("%.18f\n", result);
        }
    return 0;
}
