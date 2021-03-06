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
    var total_area=0.0;
    var comp=0.0;
    val points=new ArrayStack[Point]()
    for (i<-ticks){
      val p=new Point(i, func(i));
      points.push(p)
    }

    var right=points.pop()
    while(points.length>0){
      val left=points.top;
      val mid=midpoint(func, left, right)
      val diff=abs(left.f+right.f-mid.f*two);
      //System.out.println(left.x);
      if(diff<=eps){
        val (a, c)=neumaier_sum((left.f+right.f+mid.f*two)*(right.x-left.x)*quarter, total_area, comp);
        total_area=a;
        comp=c;
        right=points.pop();
      }else{
        points.push(mid);
      }
    }
    total_area+comp
  }
}


object Main {
  def main(args: Array[String]): Unit = {
    val precise_result=0.527038339761566009286263102166809763899326865179511011538;
    for (i <- 1 to 100) {
      val xx = Integrate.integrate(x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
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
        val xx = Integrate.integrate(x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
        //val xx = Integrate.perform_rec[Double](x => (x * x), List(0.0, 1.0), 1e-10);
        System.out.println(xx);
      }
    }
  }
}
