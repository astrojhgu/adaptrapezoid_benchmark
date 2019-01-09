#Pkg.add("DataStructures.jl")
using DataStructures

struct Interval
    x1::Float64
    f1::Float64
    x2::Float64
    f2::Float64
    subsum::Float64
    Interval(x1::Float64, f1::Float64, x2::Float64, f2::Float64)=new(x1, f1, x2, f2, (f1+f2)*(x2-x1)/2.0)

end

function split(func::Function, i::Interval)::Tuple{Interval,Interval}
    xm=(i.x1+i.x2)/2.0
    fm=func(xm)
    (Interval(i.x1,i.f1,xm,fm),Interval(xm,fm,i.x2,i.f2))
end

function try_split(func::Function, i::Interval, eps::Float64)::Tuple{Interval, Interval, Bool}
    (i1,i2)=split(func, i)
    return (i1,i2,abs(i1.subsum+i2.subsum-i.subsum)<eps)
end

function refine_iter(func::Function, unrefined::Deque{Interval}, refined::Deque{Interval}, eps::Float64, full_width::Float64)::Tuple{Deque{Interval}, Deque{Interval}}
    new_unrefined=Deque{Interval}()
    while !isempty(unrefined)
        i=pop!(unrefined)
        eps1=eps/full_width*(i.x2-i.x1)
        (i1,i2, finished)=try_split(func, i, eps1)
        if finished
            push!(refined, i1)
            push!(refined, i2)
        else
            push!(new_unrefined, i1)
            push!(new_unrefined, i2)
        end
    end
    (new_unrefined, refined)
end

function init_interval(func::Function, ticks)::Deque{Interval}
    result=Deque{Interval}()
    for i in 1:(length(ticks)-1)
        x1=ticks[i]
        x2=ticks[i+1]
        f1=func(x1)
        f2=func(x2)
        push!(result, Interval(x1,f1,x2,f2))
    end
    result
end

function refine_until_converge(func::Function, initial_intervals::Deque{Interval}, eps::Float64, full_width::Float64)::Deque{Interval}
    refined=Deque{Interval}()
    unrefined=initial_intervals
    while !isempty(unrefined)
        (unrefined, refined)=refine_iter(func, unrefined, refined, eps, full_width)
    end
    refined
end

function integrate(func::Function, init_ticks, eps::Float64)::Float64
    init_intervals=init_interval(func, init_ticks)
    full_width=last(init_ticks)-first(init_ticks)
    refined=refine_until_converge(func, init_intervals, eps, full_width)
    mapreduce(x->x.subsum, +, refined)
end

println(integrate(x->sin(x^2), [0.0, 1.0, 2.0, sqrt(8*pi)], 1e-10))
