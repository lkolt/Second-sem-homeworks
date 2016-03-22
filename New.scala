/**
  * Created by lkolt on 22.03.2016.
  */
object New {
  type Set = Int => Boolean
  def contains(s: Set, elem: Int): Boolean = s(elem)
  def singletonSet(elem: Int): Set = x => x == elem
  def union(s: Set, t: Set): Set = x => contains(s, x) || contains(t, x)
  def intersect(s: Set, t: Set): Set = x => contains(s, x) && contains(t, x)
  def diff(s: Set, t: Set): Set = x => contains(s, x) && !contains(t, x)
  def filter(s: Set, p: Int => Boolean): Set = x => contains(s, x) && p(x)

  def takeAll(s: Set): String = {
    val bound = 1000
    def toString(s: Set): String = {
      val xs = for (i <- -bound to bound if contains(s, i)) yield i
      xs.mkString("{", ",", "}")
    }
    toString(s)
  }

  def forall(s: Set, p: Int => Boolean): Boolean = {
    val bound = 1000

    @annotation.tailrec
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s, a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = {
    val bound = 1000

    @annotation.tailrec
    def iter(a: Int): Boolean = {
      if (a > bound) false
      else if (contains(s, a) && p(a)) true
      else iter(a + 1)
    }
    iter(-bound)
  }

  def map(s: Set, f: Int => Int): Set = {
    val bound = 1000

    def iter(a: Int): Set = {
      if (a > bound) x => false
      else if (contains(s, a)) union(singletonSet(f(a)), iter(a + 1))
      else iter(a + 1)
    }
    iter(-bound)
  }

  def f (x: Int) : Int = x * x
  def p(x: Int) : Boolean = x == 1

  def main(args: Array[String]): Unit = {
    val s = singletonSet(1)
    val t = union(singletonSet(1), singletonSet(2))
    val q = singletonSet(0)
    println(1, contains(s, 0) == false) // tests
    println(2, contains(s, 1) == true)
    println(3, contains(union(s, t), 2) == true)
    println(4, contains(union(s, t), 0) == false)
    println(5, contains(intersect(s, t), 2) == false)
    println(6, contains(intersect(s, t), 1) == true)
    println(7, contains(diff(t, s), 2) == true)
    println(8, contains(diff(t, s), 1) == false)
    println(9, contains(diff(t, s), 0) == false)
    println(10, contains(filter(t, p), 1) == true)
    println(11, contains(filter(t, p), 2) == false)
    println(12, forall(t, p) == false)
    println(13, forall(s, p) == true)
    println(14, exists(t, p) == true)
    println(15, exists(q, p) == false)
    println(16, contains(map(t, f), 4) == true)
    println(17, contains(map(t, f), 5) == false)
  }

}
