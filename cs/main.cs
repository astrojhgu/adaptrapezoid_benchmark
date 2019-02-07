using System.Collections;
using System.Collections.Generic;
using System;
using System.Linq;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;


namespace adapt  
{  
    struct Point{
        public double x;
        public double f;

        public Point(double x1, double f1){
            x=x1;
            f=f1;
        }
    }

    class Integration{
        public delegate double func(double x);
        
        private static Point midpoint(ref Point p1, ref Point p2, func f){
            var xm=(p1.x+p2.x)/2.0;
            return new Point(xm, f(xm));
        }

        private static (double, double) neumaier_sum(double x, double sum, double comp)
        {
            //https://en.wikipedia.org/wiki/Kahan_summation_algorithm
            double t=sum+x;
            if (Math.Abs(sum)>=Math.Abs(x)){
                comp+=((sum-t)+x);
            }else{
                comp+=((x-t)+ sum);
            }
            sum=t;
            return (sum, comp);
        }


        public static double integrate(func f,  System.Collections.Generic.IReadOnlyList<double>  init_ticks, double eps1){
            var points=new System.Collections.Generic.Stack<Point>();
            //var areas=new System.Collections.Generic.List<double>();
            double total_area=0;
            double comp=0;
            foreach(var x in init_ticks){
                points.Push(new Point(x, f(x)));
            }

            var full_width=init_ticks.Last()-init_ticks.First();
            Point right;
            double eps=eps1*4.0/full_width;
            while(points.Count>1){
                //Console.WriteLine(points.Count);
                right=points.Pop();
                var left=points.Peek();
                var mid=Integration.midpoint(ref left, ref right, f);
                if(Math.Abs(left.f+right.f-mid.f*2.0)<=eps){
                    //areas.Add((left.f+right.f+mid.f*2.0)*(right.x-left.x)/4.0);
                    double area=(left.f+right.f+mid.f*2.0)*(right.x-left.x)/4.0;
                    (total_area, comp)=neumaier_sum(total_area, area, comp);
                }else{
                    points.Push(mid);
                    points.Push(right);
                }
            }
            return total_area;
        }
    }

    public class BenchIntegration{
        private const double tol=1e-10;
        private readonly System.Collections.Generic.List<double> ticks;
        public BenchIntegration(){
            ticks=new System.Collections.Generic.List<double>(){0.0, 1.0, Math.Sqrt(Math.PI*8.0)};
            const double precise_result=0.527038339761566009286263102166809763899326865179511011538;
            
            var result=Integration.integrate(delegate(double x){return Math.Sin(x*x);}, ticks, tol);
            System.Console.WriteLine("Precision validation:");
            System.Console.Write("diff=");
            System.Console.WriteLine(Math.Abs(result-precise_result));
            System.Console.Write("Required precision=");
            System.Console.WriteLine(tol);
        }

        [Benchmark]
        public void run(){
            Integration.integrate(delegate(double x){return Math.Sin(x*x);}, ticks, tol);
        }
    }
    
    class Program  
    {  
        static void Main(string[] args)  
        {  
            var summary=BenchmarkRunner.Run<BenchIntegration>();
        }
    }  
}
