package cpp_like_no_generic;

import org.scalameter.api._

import spire.math.Fractional;
import spire.syntax.fractional._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayStack
import scala.math._

class Point(val x: Double, val f: Double) {
  override def toString={
    "("+x.toString+","+f.toString+")"
  }
}

object Integrate {
  final def midpoint(func:Double=>Double, p1:Point, p2:Point):Point={
    val xm=(p1.x+p2.x)/2;
    new Point(xm, func(xm))
  }

  final def integrate(func:Double=>Double, ticks:List[Double], eps1:Double):Double= {
    if(ticks.size<2){
      return 0.0;
    } 
    val one=1.0;
    val two=one+one;
    val four=two+two;
    val half=one/two;
    val quarter=one/four;
    val full_width=ticks.last-ticks.head;
    val eps:Double=eps1*four/full_width;
    var areas=ArrayBuffer[Double]();
    val points=new ArrayStack[Point]()
    for (i<-ticks){
      val p=new Point(i, func(i));
      points.push(p)
    }

    var right=points.top
    while(points.length>1){
      right=points.pop();
      val left=points.top;
      val mid=midpoint(func, left, right)
      val diff=abs(left.f+right.f-mid.f*two);
      //System.out.println(left.x);
      if(diff<=eps){
        areas.append((left.f+right.f+mid.f*two)*(right.x-left.x)*quarter);
      }else{
        points.push(mid);
        points.push(right);
      }
    }

    areas.sortBy(x => abs(x)).foldLeft(0.0){(a,b)=>{a+b}}
  }
}


object Main {
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 100) {
      val xx = Integrate.integrate(x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
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
        val xx = Integrate.integrate(x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
        //val xx = Integrate.perform_rec[Double](x => (x * x), List(0.0, 1.0), 1e-10);
        System.out.println(xx);
      }
    }
  }
}