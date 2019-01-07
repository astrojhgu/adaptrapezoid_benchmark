import spire.math.Fractional;
import spire.syntax.fractional._


import scala.math._

class Interval[T: Fractional : Ordering](val x1: T, val f1: T, val x2: T, val f2: T) {
  /*             
  * |        _.*|
  * |     _.*   |
  * |   .*      f2
  * |  |        |
  * |  f1 subsum|
  * |__|________|__________________
  *    x1       x2
  *
  * so that subsum=(f1+f2)*(x2-x1)/2
  * */
  val subsum = (f1 + f2) * (x2 - x1) / 2.0;

  def split(func: T => T): (Interval[T], Interval[T]) = {
    /*
    * split an interval into two
    * */
    val xm = (x1 + x2) / (Fractional[T].one+Fractional[T].one);
    val fm = func(xm);
    (new Interval[T](x1, f1, xm, fm), new Interval[T](xm, fm, x2, f2))
  }

  def try_refine(func: T => T, eps: T, full_width: T): Either[(Interval[T], Interval[T]), Interval[T]] = {
    /*
    * if the difference between subsum of current interval and the sum of subsum's of splitted intervals
    * is less than a threshold (i.e., eps), then this interval is done, otherwise the splitted result is returned
    * */
    val width = this.x2 - this.x1;
    val eps1 = eps * width / full_width;
    val (i1, i2) = this.split(func);
    if (Fractional[T].abs(i1.subsum + i2.subsum - this.subsum) > eps1) {
      Left((i1, i2))
    } else {
      Right(this)
    }
  }
}

object Interval {
  final def refine_iter[T: Fractional : Ordering](func: T => T, previous: (List[Interval[T]], List[Interval[T]]), eps: T, full_width: T): (List[Interval[T]], List[Interval[T]]) = {
    /*
    * previous._1 is the list of intervals that has not yet been done
    * previous._2 is the list of intervals that has been done
    * The return value has the same structure
    * */


    var need_more_refine = List[Interval[T]]();
    var refined = previous._2;
    for (i <- previous._1) {
      i.try_refine(func, eps, full_width) match {
        case Left((i1, i2)) => {
          need_more_refine = i1 :: i2 :: need_more_refine
        }
        case Right(i) => {
          refined = i :: refined
        }
      }
    }
    (need_more_refine, refined)
  }

  final def init_interval[T: Fractional : Ordering](func: T => T, init_ticks: List[T]): List[Interval[T]] = {
    for ((x1, x2) <- init_ticks.init.zip(init_ticks.tail)) yield new Interval[T](x1, func(x1), x2, func(x2))
  }

  final def refine_until_converge[T: Fractional : Ordering](func: T => T, init_ticks: List[T], eps: T): T = {
    var intervals = this.init_interval(func, init_ticks);
    val full_width = init_ticks.last - init_ticks.head;
    var refined = List[Interval[T]]();
    while (!intervals.isEmpty) {
      val (i1, ss1) = refine_iter(func, (intervals, refined), eps, full_width);
      intervals = i1;
      refined = ss1;
    }
    var s = 0.0;
    refined.sortBy(x => Fractional[T].abs(x.subsum)).foldLeft(Fractional[T].zero) { (a, b) => a + b.subsum }
  }
}


object Main {
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 100) {
      val xx = Interval.refine_until_converge[Double](x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
      System.out.println(xx);
    }
  }
}
