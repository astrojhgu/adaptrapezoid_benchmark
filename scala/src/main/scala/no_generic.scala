package no_generic;

import org.scalameter.api._

import scala.annotation.tailrec

import scala.math._

class Point(val x: Double, val f: Double) {

}

object Integrate {
  final def midpoint(func:Double=>Double, p1:Point, p2:Point):Point={
    val xm=(p1.x+p2.x)/2;
    new Point(xm, func(xm))
  }

  final def area(p1:Point, p2:Point):Double={
    (p1.f+p2.f)*(p2.x-p1.x)/2
  }

  final def neumaier_sum(x:Double, sum:Double, comp:Double):(Double, Double)={
    //https://en.wikipedia.org/wiki/Kahan_summation_algorithm
    val t=sum+x;
    val comp1=
    if (abs(sum)>=abs(x)){
        comp+((sum-t)+x);
    }else{
        comp+((x-t)+ sum);
    };
    val sum1=t;
    (sum1, comp1)
  }



  @tailrec
  final def integrate_rec(func:Double=>Double, eps:Double, points:List[Point], total_area:Double, comp:Double):(List[Point], Double, Double)={
    points match {
      case left::right::others=>{
        val midpoint=this.midpoint(func, left, right)
        if (abs(left.f+right.f-midpoint.f*2)<=eps){
          val (a, c)=neumaier_sum(area(left, right), total_area, comp);
          integrate_rec(func, eps, right::others, a, c)
        }else{
          integrate_rec(func, eps, left::midpoint::right::others, total_area, comp)
        }
      }
      case _ =>(points, total_area, comp)
    }
  }

  final def perform_rec(func:Double=>Double, init_ticks:List[Double], eps:Double):Double={
    val points=init_ticks.map(x=>new Point(x, func(x)))
    val eps1=eps*4/(init_ticks.last-init_ticks.head)
    val (_, total_area, comp)=integrate_rec(func, eps1, points, 0.0, 0.0)
    total_area+comp
  }

  final def integrate_iter(func:Double=>Double, eps:Double, points:List[Point], total_area:Double, comp:Double):(List[Point], Double, Double)= {
    points match {
      case left::right::others=>{
        val midpoint=this.midpoint(func, left, right)
        if (abs(left.f+right.f-midpoint.f*2)<=eps){
          val (a, c)=neumaier_sum(area(left, right), total_area, comp)
          (right::others, a, c)
        }else{
          (left::midpoint::right::others, total_area, comp)
        }
      }
      case _ =>(points, total_area, comp)
    }
  }

  final def perform_iter(func:Double=>Double, init_ticks:List[Double], eps:Double):Double={
    var points=init_ticks.map(x=>new Point(x, func(x)))
    val eps1=eps*4/(init_ticks.last-init_ticks.head)
    var total_area=0.0;
    var comp=0.0;

    while (points.size>1){
      val result=integrate_iter(func, eps1, points, total_area, comp)
      points=result._1
      total_area=result._2
      comp=result._3;
    }
    total_area+comp
  }

}


object Main {
  def main(args: Array[String]): Unit = {
    val precise_result=0.527038339761566009286263102166809763899326865179511011538;
    for (i <- 1 to 100) {
      val xx = Integrate.perform_iter(x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
      //val xx = Integrate.perform_rec[Double](x => (x * x), List(0.0, 1.0), 1e-10);
      System.out.print("diff=");
      System.out.println(abs(precise_result-xx));
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
        val xx = Integrate.perform_iter(x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
        //val xx = Integrate.perform_rec[Double](x => (x * x), List(0.0, 1.0), 1e-10);
        System.out.println(xx);
      }
    }
  }

  performance of "Integrate_rec" in {
    using (funcs) in {
      Unit=>{
        val xx = Integrate.perform_rec(x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
        //val xx = Integrate.perform_rec[Double](x => (x * x), List(0.0, 1.0), 1e-10);
        System.out.println(xx);
      }
    }
  }
}
