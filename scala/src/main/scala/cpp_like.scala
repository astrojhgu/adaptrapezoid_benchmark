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
    var total_area=Fractional[T].zero;
    var comp=Fractional[T].zero;
    val points=new ArrayStack[Point[T]]()
    for (i<-ticks){
      val p=new Point[T](i, func(i));
      points.push(p)
    }

    var right=points.pop()
    while(points.length>0){
      val left=points.top;
      val mid=midpoint(func, left, right)
      val diff=Fractional[T].abs(left.f+right.f-mid.f*two);
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
      val xx = Integrate.integrate[Double](x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
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
        val xx = Integrate.integrate[Double](x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
        //val xx = Integrate.perform_rec[Double](x => (x * x), List(0.0, 1.0), 1e-10);
        System.out.println(xx);
      }
    }
  }
}
