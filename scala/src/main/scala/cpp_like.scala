package cpp_like;

import org.scalameter.api._

import spire.math.Fractional;
import spire.syntax.fractional._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayStack
import scala.math._

class Point[T:Fractional](val x: T, val f: T) {
  override def toString={
    "("+x.toString+","+f.toString+")"
  }
}

object Integrate {
  final def midpoint[T:Fractional](func:T=>T, p1:Point[T], p2:Point[T]):Point[T]={
    val xm=(p1.x+p2.x)/2;
    new Point[T](xm, func(xm))
  }

  final def integrate[T:Fractional:Ordering](func:T=>T, ticks:List[T], eps1:T):T= {
    if(ticks.size<2){
      return Fractional[T].zero;
    } 
    val one=Fractional[T].one;
    val two=one+one;
    val four=two+two;
    val half=one/two;
    val quarter=one/four;
    val full_width=ticks.last-ticks.head;
    val eps:T=eps1*four/full_width;
    var areas=ArrayBuffer[T]();
    val points=new ArrayStack[Point[T]]()
    for (i<-ticks){
      val p=new Point[T](i, func(i));
      points.push(p)
    }

    var right=points.top
    while(points.length>1){
      right=points.pop();
      val left=points.top;
      val mid=midpoint(func, left, right)
      val diff=Fractional[T].abs(left.f+right.f-mid.f*two);
      //System.out.println(left.x);
      if(diff<=eps){
        areas.append((left.f+right.f+mid.f*two)*(right.x-left.x)*quarter);
      }else{
        points.push(mid);
        points.push(right);
      }
    }

    areas.sortBy(x => Fractional[T].abs(x)).foldLeft(Fractional[T].zero){(a,b)=>{a+b}}
  }
}


object Main {
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 100) {
      val xx = Integrate.integrate[Double](x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
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
        val xx = Integrate.integrate[Double](x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
        //val xx = Integrate.perform_rec[Double](x => (x * x), List(0.0, 1.0), 1e-10);
        System.out.println(xx);
      }
    }
  }
}