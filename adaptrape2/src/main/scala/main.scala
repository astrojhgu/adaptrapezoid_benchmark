import spire.math.Fractional;
import spire.syntax.fractional._
import scala.annotation.tailrec

import scala.math._

class Point[T:Fractional](val x: T, val f: T) {

}

object Integrate {
  final def integrate[T:Fractional:Ordering](func:T=>T, eps:T, init_ticks:Seq[T]){}

  final def midpoint[T:Fractional](func:T=>T, p1:Point[T], p2:Point[T]):Point[T]={
    val xm=(p1.x+p2.x)/2;
    new Point[T](xm, func(xm))
  }

  final def area[T:Fractional](p1:Point[T], p2:Point[T]):T={
    (p1.f+p2.f)*(p2.x-p1.x)/2
  }


  @tailrec
  final def integrate_rec[T:Fractional:Ordering](func:T=>T, eps:T, points:List[Point[T]], areas:List[T]):(List[Point[T]], List[T])={
    points match {
      case left::right::others=>{
        val midpoint=this.midpoint(func, left, right)
        if (Fractional[T].abs(left.f+right.f-midpoint.f*2)<=eps){
          integrate_rec(func, eps, right::others, area(left, right)::areas)
        }else{
          integrate_rec(func, eps, left::midpoint::right::others, areas)
        }
      }
      case _ =>(points, areas)
    }
  }

  final def perform_rec[T:Fractional:Ordering](func:T=>T, init_ticks:List[T], eps:T):T={
    val points=init_ticks.map(x=>new Point[T](x, func(x)))
    val eps1=eps*4/(init_ticks.last-init_ticks.head)
    val areas=integrate_rec(func, eps1, points, Nil)._2
    areas.sortBy(x => Fractional[T].abs(x)).foldLeft(Fractional[T].zero){(a,b)=>{a+b}}
  }

  final def integrate_iter[T:Fractional:Ordering](func:T=>T, eps:T, points:List[Point[T]], areas:List[T]):(List[Point[T]], List[T])= {
    points match {
      case left::right::others=>{
        val midpoint=this.midpoint(func, left, right)
        if (Fractional[T].abs(left.f+right.f-midpoint.f*2)<=eps){
          (right::others, area(left, right)::areas)
        }else{
          (left::midpoint::right::others, areas)
        }
      }
      case _ =>(points, areas)
    }
  }

  final def perform_iter[T:Fractional:Ordering](func:T=>T, init_ticks:List[T], eps:T):T={
    var points=init_ticks.map(x=>new Point[T](x, func(x)))
    val eps1=eps*4/(init_ticks.last-init_ticks.head)
    var areas:List[T]=Nil

    while (points.size>1){
      val result=integrate_iter(func, eps1, points, areas)
      points=result._1
      areas=result._2
    }
    areas.sortBy(x => Fractional[T].abs(x)).foldLeft(Fractional[T].zero){(a,b)=>{a+b}}
  }

}


object Main {
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 100) {
      //val xx = Integrate.perform_rec[Double](x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
      val xx = Integrate.perform_rec[Double](x => (x * x), List(0.0, 1.0), 1e-10);
      System.out.println(xx);
    }
  }
}
