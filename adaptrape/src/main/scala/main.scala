import scala.math._

class Interval(val x1:Double, val f1:Double, val x2:Double, val f2:Double){
  val subsum=(f1+f2)*(x2-x1)/2.0;

  def split(func:Double=>Double):(Interval, Interval)={
    val xm=(x1+x2)/2.0;
    val fm=func(xm);
    (new Interval(x1, f1, xm, fm), new Interval(xm, fm, x2, f2))
  }

  def try_refine(func:Double=>Double, eps:Double, full_width:Double):Either[(Interval, Interval), Interval]={
    val width=this.x2-this.x1;
    val eps1=eps*width/full_width;
    val (i1, i2)=this.split(func);
    if (abs(i1.subsum+i2.subsum-this.subsum)>eps1){
      Left((i1, i2))
    }else{
      Right(this)
    }
  }
}

object Interval{
  final def refine_iter(func:Double=>Double, previous:(List[Interval], List[Interval]), eps:Double, full_width:Double): (List[Interval], List[Interval]) ={
    var need_more_refine=List[Interval]();
    var refined=previous._2;
    for(i<-previous._1){
      i.try_refine(func, eps, full_width) match {
        case Left((i1,i2))=>{need_more_refine=i1::i2::need_more_refine}
        case Right(i)=>{refined=i::refined}
      }
    }
    (need_more_refine, refined)
  }

  final def init_interval(func:Double=>Double, x1:Double, x2:Double):Interval=new Interval(x1, func(x1), x2, func(x2))

  final def init_interval(func:Double=>Double, init_ticks:List[Double]):List[Interval]={
    for((x1,x2)<-init_ticks.init.zip(init_ticks.tail))  yield new Interval(x1, func(x1), x2, func(x2))
  }

  final def refine_until_converge(func:Double=>Double, init_ticks:List[Double], eps:Double):Double={
    var intervals=this.init_interval(func, init_ticks);
    val full_width=init_ticks.last-init_ticks.head;
    var refined=List[Interval]();
    while(!intervals.isEmpty){
      val (i1, ss1)=refine_iter(func, (intervals, refined), eps, full_width);
      intervals=i1;
      refined=ss1;
    }
    var s=0.0;
    refined.sortBy(x=>abs(x.subsum)).foldLeft(0.0){(a,b)=>a+b.subsum}
  }
}


object Main{
  def main(args:Array[String]): Unit ={
    for(i<-1 to 10) {
      val xx = Interval.refine_until_converge(x => sin(x * x), List(0.0, 1.0, 2.0, sqrt(8.0 * scala.math.Pi)), 1e-10);
      System.out.println(xx);
    }
  }
}
