#include <math.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#define STACK_CAP 1024
#define VEC_CAP 2097152

typedef struct
{
    double x;
    double f;
} Point;

Point midpoint (double (*func) (double), Point p1, Point p2)
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

void extend_stack (PointStack *ps, int new_cap)
{
    ps->mem = (Point *)realloc (ps->mem, new_cap * sizeof (Point));
    assert (ps->mem != 0);
    ps->cap = new_cap;
    // printf("extended\n");
}

void push (PointStack *ps, Point p)
{
    if (ps->top_idx >= ps->cap - 1)
        {
            extend_stack (ps, ps->cap * 2);
        }
    ++ps->top_idx;
    ps->mem[ps->top_idx] = p;
}

Point pop (PointStack *ps)
{
    return ps->mem[ps->top_idx--];
}

Point top (PointStack *ps)
{
    return ps->mem[ps->top_idx];
}

int stack_size (PointStack *ps)
{
    return ps->top_idx + 1;
}

PointStack init_point_stack (int cap)
{
    Point *mem = (Point *)malloc (sizeof (Point) * cap);
    PointStack pp = { mem, cap, -1 };
    return pp;
}

void finalize_point_stack (PointStack *ps)
{
    free (ps->mem);
    ps->mem = 0;
    ps->cap = 0;
    ps->top_idx = -1;
}

//////////////DoubleVec//////////////
typedef struct
{
    double *mem;
    int cap;
    int size;
} DoubleVec;

void finalize_double_vec (DoubleVec *dv)
{
    free (dv->mem);
    dv->cap = 0;
    dv->size = 0;
    dv->mem = 0;
}

void extend_double_vec (DoubleVec *dv, int new_cap)
{
    dv->mem = (double *)realloc (dv->mem, new_cap * sizeof (double));
    assert (dv->mem != 0);
    dv->cap = new_cap;
    // printf("extended to %d\n", new_cap);
    // printf("extended1\n");
}

DoubleVec init_double_vec (int cap)
{
    DoubleVec dv;
    dv.mem = (double *)malloc (cap * sizeof (double));
    dv.cap = cap;
    dv.size = 0;
    return dv;
}

double nth_element (DoubleVec *dv, int i)
{
    return dv->mem[i];
}

void append_element (DoubleVec *dv, double x)
{
    if (dv->size == dv->cap)
        {
            extend_double_vec (dv, dv->cap * 2);
            // printf("cap=%d\n", dv->cap);
        }
    dv->mem[dv->size++] = x;
}

int comp (const void *_x1, const void *_x2)
{
    double x1 = fabs (*(double *)_x1);
    double x2 = fabs (*(double *)_x2);
    return x1==x2?0:(x1>x2)?1:-1;
}

double integrate (double (*func) (double), double *init_ticks, int nticks, double eps1)
{
    if (nticks <= 1)
        {
            return 0;
        }
    double full_width = init_ticks[nticks - 1] - init_ticks[0];
    // double area=0;
    DoubleVec areas = init_double_vec (VEC_CAP);
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
                    // area+=s
                    append_element (&areas, s);
                }
            else
                {
                    push (&ps, midp);
                    push (&ps, right);
                }
        }
    qsort (areas.mem, areas.size, sizeof (double), comp);
    double area = 0;
    for (int i = 0; i < areas.size; ++i)
        {
            area += nth_element (&areas, i);
        }
    finalize_point_stack (&ps);
    finalize_double_vec (&areas);
    return area;
}

double foo (double x)
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
