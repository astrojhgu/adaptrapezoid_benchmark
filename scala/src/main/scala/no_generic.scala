package no_generic;

import org.scalameter.api._

import scala.annotation.tailrec

import scala.math._

class Point(val x: Double, val f: Double) {

}

object Integrate {
  final def integrate(func:Double=>Double, eps:Double, init_ticks:Seq[Double]){}

  final def midpoint(func:Double=>Double, p1:Point, p2:Point):Point={
    val xm=(p1.x+p2.x)/2;
    new Point(xm, func(xm))
  }

  final def area(p1:Point, p2:Point):Double={
    (p1.f+p2.f)*(p2.x-p1.x)/2
  }


  @tailrec
  final def integrate_rec(func:Double=>Double, eps:Double, points:List[Point], areas:List[Double]):(List[Point], List[Double])={
    points match {
      case left::right::others=>{
        val midpoint=this.midpoint(func, left, right)
        if (abs(left.f+right.f-midpoint.f*2)<=eps){
          integrate_rec(func, eps, right::others, area(left, right)::areas)
        }else{
          integrate_rec(func, eps, left::midpoint::right::others, areas)
        }
      }
      case _ =>(points, areas)
    }
  }

  final def perform_rec(func:Double=>Double, init_ticks:List[Double], eps:Double):Double={
    val points=init_ticks.map(x=>new Point(x, func(x)))
    val eps1=eps*4/(init_ticks.last-init_ticks.head)
    val areas=integrate_rec(func, eps1, points, Nil)._2
    areas.sortBy(x => abs(x)).foldLeft(0.0){(a,b)=>{a+b}}
  }

  final def integrate_iter(func:Double=>Double, eps:Double, points:List[Point], areas:List[Double]):(List[Point], List[Double])= {
    points match {
      case left::right::others=>{
        val midpoint=this.midpoint(func, left, right)
        if (abs(left.f+right.f-midpoint.f*2)<=eps){
          (right::others, area(left, right)::areas)
        }else{
          (left::midpoint::right::others, areas)
        }
      }
      case _ =>(points, areas)
    }
  }

  final def perform_iter(func:Double=>Double, init_ticks:List[Double], eps:Double):Double={
    var points=init_ticks.map(x=>new Point(x, func(x)))
    val eps1=eps*4/(init_ticks.last-init_ticks.head)
    var areas:List[Double]=Nil

    while (points.size>1){
      val result=integrate_iter(func, eps1, points, areas)
      points=result._1
      areas=result._2
    }
    areas.sortBy(x => abs(x)).foldLeft(0.0){(a,b)=>{a+b}}
  }

}


object Main {
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 100) {
      val xx = Integrate.perform_iter(x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
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