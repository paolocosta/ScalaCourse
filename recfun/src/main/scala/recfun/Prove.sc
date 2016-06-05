import scala.annotation.tailrec

object sample {
  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)

  def product(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f, a + 1, b)

  def mapReduce(f: Int => Int, operation: (Int, Int) => Int, initial:Int) (a:Int, b:Int):Int =
    if(a>b) initial
    else operation(f(a), mapReduce(f, operation, initial) (a+1, b))

  def fatc(a: Int): Int =
    product(x=> x,1,a)

  def sum2(f: Int => Int, a: Int, b: Int): Int = {
    @tailrec
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))

    }
    loop(a, 0)
  }

  def sum3(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a, b)

    sumF
  }


  println(mapReduce(x => x, (x,y) => x + y, 0)(3,6))
  var r = new Rational(2)
  println(r.denominator)

  class Rational(x:Int, y:Int)
  {
    require(y > 0, "denominator = 0")

    def this(x:Int) = this(x,1)
    def numerator = x
    def denominator = y

  }
}



