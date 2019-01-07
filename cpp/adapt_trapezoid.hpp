#ifndef ADAPT_TRAPEZOID
#define ADAPT_TRAPEZOID

#include <fstream>
#include <tuple>
#include <functional>
#include <cmath>
#include <optional>
#include <list>
#include <utility>
#include <algorithm>
#include <iostream>
template <typename T>
struct Interval{
    T x1;
    T f1;
    T x2;
    T f2;
    T subsum;

    bool operator<(const Interval<T>& rhs)const{
        return subsum<rhs.subsum;
    }

    std::optional<std::pair<Interval<T>, Interval<T> > > try_refine(const std::function<T(T)>& func, 
                                                T eps){
        T xm=(x1+x2)/static_cast<T>(2);
        T fm=func(xm);
        T ss1=(f1+fm)*(xm-x1)/static_cast<T>(2);
        T ss2=(fm+f2)*(x2-xm)/static_cast<T>(2);
        if(std::abs(ss1+ss2-subsum)<=eps){
            return std::optional<std::pair<Interval<T>, Interval<T> > >();
        }
        else
        {
            auto i1=Interval{x1, f1, xm, fm, ss1};
            auto i2=Interval{xm, fm, x2, f2, ss2};
            return std::make_optional(std::make_pair(i1, i2));
        }
        
    }
};

template <typename T>
void refine_iter(const std::function<T(T)>& func, T eps, std::list<Interval<T> >& intervals, 
                std::list<Interval<T> >& refined,T original_width)
{
    std::list<Interval<T> > next_intervals;
    while(!intervals.empty()){
        auto i=intervals.front();
        auto interval_width=i.x2-i.x1;
        auto ii=i.try_refine(func, eps/original_width*interval_width);
        if (ii.has_value()){
            Interval<T> i1, i2;
            std::tie(i1, i2)=ii.value();
            next_intervals.push_back(i1);
            next_intervals.push_back(i2);
        }else{
            refined.push_back(i);
        }
        intervals.pop_front();
    }
    intervals.swap(next_intervals);
}

template <typename T>
T sum_up(std::list<Interval<T> >& refined){
    T result=static_cast<T>(0);
    std::vector<Interval<T>> sorted;
    sorted.reserve(refined.size());
    for(auto& i:refined){
        sorted.push_back(i);
    }
    std::sort(sorted.begin(), sorted.end());
    for(auto& i:sorted){
        result+=i.subsum;
    }
    return result;
}

template <typename T>
std::list<Interval<T> > refine_until_converged(
    const std::function<T(T)>& func, std::list<Interval<T> >& init_list, 
    T eps)
{
    auto original_width=init_list.back().x2-init_list.front().x1;
    std::list<Interval<T> > result;
    while(!init_list.empty()){
        refine_iter(func, eps, init_list, result, original_width);
    }
    return result;
}

template<typename T>
T integrate(const std::function<T(T)>& func, 
            T eps,std::vector<T> init_ticks)
{
    std::list<Interval<T> > init_list;
    for(size_t i=0;i<init_ticks.size()-1;++i){
        T x1=init_ticks[i];
        T x2=init_ticks[i+1];
        T f1=func(x1);
        T f2=func(x2);
        T subsum=(f1+f2)*(x2-x1)/static_cast<T>(2);
        Interval<T> ii{x1, f1, x2, f2, subsum};
        init_list.push_back(ii);
    }
    auto pp=refine_until_converged(func, init_list, eps);
    
    return sum_up(pp);
}

#endif
