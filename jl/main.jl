#!/usr/bin/env julia

struct Point{T<:AbstractFloat}
    x::T
    f::T
    #Point{T}(x::T, f::T)=new{T}(x::T, f::T)
end

function integrate(func::Function, ticks::Array{T,1}, eps::T)::T where T<:AbstractFloat
    if length(ticks)<2
        return 0.0
    end
    points::Array{Point{T}, 1}=[Point(x, func(x)) for x in ticks]
    full_width=last(ticks)-first(ticks)
    areas::Array{T,1}=[]
    right=last(points)
    sz=length(points)
    while sz>1 
        left=points[lastindex(points)-1]
        mid=(left.x+right.x)/2
        fmid=func(mid)
        if abs(left.f+right.f-fmid*2.0)<=eps
            push!(areas, (left.f+right.f+fmid*2.0)*(right.x-left.x)/4.0)
            pop!(points)
            right=left
            sz-=1
        else
            pop!(points)
            push!(points, Point(mid, fmid))
            push!(points, right)
            sz+=1
        end
    end
    areas=sort!(areas, by=x->abs(x))
    sum(areas)
end

#for i in 1:100
#@time for i in 1:100;  println(integrate(x->sin(x^2), [0.0, 1.0, 2.0, sqrt(8*pi)], 1e-10)); end
#end
