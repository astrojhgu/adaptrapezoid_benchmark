package generic;

import org.scalameter.api._

import spire.math.Fractional;
import spire.syntax.fractional._
import scala.annotation.tailrec

import scala.math._

class Point[T:Fractional](val x: T, val f: T) {

}

object Integrate {
  final def midpoint[T:Fractional](func:T=>T, p1:Point[T], p2:Point[T]):Point[T]={
    val xm=(p1.x+p2.x)/2;
    new Point[T](xm, func(xm))
  }

  final def area[T:Fractional](p1:Point[T], p2:Point[T]):T={
    (p1.f+p2.f)*(p2.x-p1.x)/2
  }

  final def neumaier_sum[T:Fractional](x:T, sum:T, comp:T):(T,T)={
      //https://en.wikipedia.org/wiki/Kahan_summation_algorithm
      val t=sum+x;
      val comp1=
      if (Fractional[T].abs(sum)>=Fractional[T].abs(x)){
          comp+((sum-t)+x);
      }else{
          comp+((x-t)+ sum);
      };
      val sum1=t;
      (sum1, comp1)
  }


  @tailrec
  final def integrate_rec[T:Fractional:Ordering](func:T=>T, eps:T, points:List[Point[T]], total_area:T, comp:T):(List[Point[T]], T, T)={
    points match {
      case left::right::others=>{
        val midpoint=this.midpoint(func, left, right)
        if (Fractional[T].abs(left.f+right.f-midpoint.f*2)<=eps){
          val (a, c)=neumaier_sum(area(left, right), total_area, comp);
          integrate_rec(func, eps, right::others, a, c)
        }else{
          integrate_rec(func, eps, left::midpoint::right::others, total_area, comp)
        }
      }
      case _ =>(points, total_area, comp)
    }
  }

  final def perform_rec[T:Fractional:Ordering](func:T=>T, init_ticks:List[T], eps:T):T={
    val points=init_ticks.map(x=>new Point[T](x, func(x)))
    val eps1=eps*4/(init_ticks.last-init_ticks.head)
    val (_, total_area, comp)=integrate_rec(func, eps1, points, Fractional[T].zero, Fractional[T].zero);
    total_area+comp
  }

  final def integrate_iter[T:Fractional:Ordering](func:T=>T, eps:T, points:List[Point[T]], total_area:T, comp:T):(List[Point[T]], T, T)= {
    points match {
      case left::right::others=>{
        val midpoint=this.midpoint(func, left, right)
        if (Fractional[T].abs(left.f+right.f-midpoint.f*2)<=eps){
          val (a, c)=neumaier_sum(area(left, right), total_area, comp);
          (right::others, a, c)
        }else{
          (left::midpoint::right::others, total_area, comp)
        }
      }
      case _ =>(points, total_area, comp)
    }
  }

  final def perform_iter[T:Fractional:Ordering](func:T=>T, init_ticks:List[T], eps:T):T={
    var points=init_ticks.map(x=>new Point[T](x, func(x)))
    val eps1=eps*4/(init_ticks.last-init_ticks.head)
    var total_area=Fractional[T].zero;
    var comp=Fractional[T].zero;

    while (points.size>1){
      val result=integrate_iter(func, eps1, points, total_area, comp)
      points=result._1
      total_area=result._2
      comp=result._3
    }
    total_area+comp
  }

}


object Main {
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 100) {
      val xx = Integrate.perform_iter[Double](x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
      //val xx = Integrate.perform_rec[Double](x => (x * x), List(0.0, 1.0), 1e-10);
      System.out.println(xx);
    }
  }
}


object MyBench
  extends Bench.LocalTime {

  //val sizes = Gen.range("size")(0, 0, 1)
  val funcs= Gen.unit("Integrating")

  // multiple tests can be specified here
  performance of "Integrate_iter" in {
    using (funcs) in {
      Unit=>{
        val xx = Integrate.perform_iter[Double](x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
        //val xx = Integrate.perform_rec[Double](x => (x * x), List(0.0, 1.0), 1e-10);
        System.out.println(xx);
      }
    }
  }

  performance of "Integrate_rec" in {
    using (funcs) in {
      Unit=>{
        val xx = Integrate.perform_rec[Double](x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
        //val xx = Integrate.perform_rec[Double](x => (x * x), List(0.0, 1.0), 1e-10);
        System.out.println(xx);
      }
    }
  }
}
