/*
Stealed from
https://gist.github.com/Eastsun/0711bbc985be15a63ae8b3a3aaddfe34
*/

import spire.algebra._
import spire.math._
import spire.implicits._

class Interval[V: Fractional](x1: V, f1: V, x2: V, f2: V) {

  val area: V = (f1 + f2) * (x2 - x1) / 2.0

  def compute(f: V => V, eps: V, width: V): V = {
    val xm = (x1 + x2) / 2.0
    val fm = f(xm)
    val i1 = Interval(x1, f1, xm, fm)
    val i2 = Interval(xm, fm, x2, f2)
    if (abs(i1.area + i2.area - this.area) <= eps * (x2 - x1) / width) {
      i1.area + i2.area
    }
    else {
      i1.compute(f, eps, width) + i2.compute(f, eps, width)
    }
  }
}

object Interval {
  def apply[V: Fractional](x1: V, f1: V, x2: V, f2: V): Interval[V] = {
    new Interval(x1, f1, x2, f2)
  }

  def integrate[V: Fractional](f: V => V, ticks: Seq[V], eps: V = 1e-10): V = {
    val width = ticks.last - ticks.head
    (for ((x1, x2) <- ticks.zip(ticks.tail))
      yield Interval(x1, f(x1), x2, f(x2)).compute(f, eps, width)).reduce(_+_)
  }
}

object Main2 {
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 100) {
      val r = Interval.integrate[Double](x => sin(x * x), Seq(0.0, 1.0, 2.0, sqrt(8 * scala.math.Pi)))
      println(r)
    }
  }
}
