#ifndef ADAPT_TRAPEZOID
#define ADAPT_TRAPEZOID

#include <fstream>
#include <tuple>
#include <functional>
#include <cmath>
#include <optional>
#include <vector>
#include <deque>
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

    Interval()=default;
    Interval(T _x1, T _f1, T _x2, T _f2)
    :x1(_x1), f1(_f1), x2(_x2), f2(_f2), subsum((f1+f2)*(x2-x1)/2){
    }
    ~Interval()=default;
    Interval(const Interval&)=default;
    Interval<T>& operator=(const Interval<T>&)=default;

    bool operator<(const Interval<T>& rhs)const{
        return std::abs(subsum)<std::abs(rhs.subsum);
    }

    std::pair<Interval<T>, Interval<T>> split(const std::function<T(T)>& func)const{
        T xm=(x1+x2)/static_cast<T>(2);
        T fm=func(xm);
        T ss1=(f1+fm)*(xm-x1)/static_cast<T>(2);
        T ss2=(fm+f2)*(x2-xm)/static_cast<T>(2);
        return std::make_pair(Interval<T>(x1, f1, xm, fm), Interval<T>(xm, fm, x2, f2));
    }

    std::tuple<std::pair<Interval<T>, Interval<T>>, bool> try_refine(const std::function<T(T)>& func, 
                                                T eps){
        auto splited=split(func);
        auto& i1=splited.first;
        auto& i2=splited.second;
        
        if(std::abs(i1.subsum+i2.subsum-subsum)<=eps){
            return std::make_tuple(splited, true);
        }
        else
        {
            return std::make_tuple(splited, false);
        }
        
    }
};

template <typename T>
void refine_iter(const std::function<T(T)>& func, T eps, std::deque<Interval<T> >& intervals, 
                std::deque<Interval<T> >& refined,T original_width)
{
    std::deque<Interval<T> > next_intervals;
    while(!intervals.empty()){
        auto i=intervals.front();
        auto interval_width=i.x2-i.x1;
        auto ii=i.try_refine(func, eps/original_width*interval_width);
        if (!std::get<1>(ii)){
            next_intervals.push_back(std::get<0>(ii).first);
            next_intervals.push_back(std::get<0>(ii).second);
        }else{
            refined.push_back(std::get<0>(ii).first);
            refined.push_back(std::get<0>(ii).second);
        }
        intervals.pop_front();
    }
    intervals.swap(next_intervals);
}

template <typename T>
T sum_up(std::deque<Interval<T> >& refined){
    std::sort(refined.begin(), refined.end());
    T result=static_cast<T>(0);
    for(auto& i:refined){
        result+=i.subsum;
    }
    return result;
}

template <typename T>
std::deque<Interval<T> > refine_until_converged(
    const std::function<T(T)>& func, std::deque<Interval<T> >& init_list, 
    T eps)
{
    auto original_width=init_list.back().x2-init_list.front().x1;
    std::deque<Interval<T> > result;
    while(!init_list.empty()){
        refine_iter(func, eps, init_list, result, original_width);
    }
    return result;
}

template<typename T>
T integrate(const std::function<T(T)>& func, 
            T eps,std::vector<T> init_ticks)
{
    std::deque<Interval<T> > init_list;
    for(size_t i=0;i<init_ticks.size()-1;++i){
        T x1=init_ticks[i];
        T x2=init_ticks[i+1];
        T f1=func(x1);
        T f2=func(x2);
        Interval<T> ii(x1, f1, x2, f2);
        init_list.push_back(ii);
    }
    auto pp=refine_until_converged(func, init_list, eps);
    
    return sum_up(pp);
}

#endif
