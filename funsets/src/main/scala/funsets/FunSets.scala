package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Pippo = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Pippo, elem: Int): Boolean = s(elem)

  val t = Set(8)
  val t2 = Pippo(8)
  println(t)
  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: Int): Pippo = x => (x==elem)
    def all: Pippo = x => true
    def none: Pippo = x => false
    def even: Pippo = x => (x % 2 == 0)
    def odd: Pippo = x => (x % 2 == 1)

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Pippo, t: Pippo): Pippo = x => contains(s, x) || contains(t,x)
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Pippo, t: Pippo): Pippo = x => contains(s, x) && contains(t,x)
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Pippo, t: Pippo): Pippo = x => contains(s, x) && !contains(t,x)
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Pippo, p: Int => Boolean): Pippo = x => contains(s, x) && p(x)
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Pippo, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a>1000) true
      else if (s(a) && !p(a)) false
      else iter(a+1)
    }
    iter(-1000)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Pippo, p: Int => Boolean): Boolean = !forall(x=> s(x), x => !p(x))
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Pippo, f: Int => Int): Pippo = x=> exists(s, y => f(y) == x)
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Pippo): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Pippo) {
    println(toString(s))
  }
}
