package org.playwithscala

object FunSets {

  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)

  def emptySet: Set = x => false

  def singletonSet(elem: Int): Set = x => x == elem
  //def singletonSet(elem: Int) : Int => Boolean = x => x == elem

  def union(s: Set, t: Set): Set = x => s(x) || t(x)

  def intersect(s: Set, t: Set): Set = x => s(x) && t(x)

  def diff(s: Set, t: Set): Set = x => s(x) && !t(x)

  def filter(s: Set, p: Int => Boolean): Set = intersect(s, p)

  val bound = 1000

  def forall(s: Set, p: Int => Boolean): Boolean = {
    // If ALL elements in S satisfy P
    def iter(a: Int): Boolean = { // iterate to limit
      if (a > bound) true
      else if(contains(s, a) && !p(a)) false
      else iter(a + 1) 
    }
    iter(-bound)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  def map(s: Set, f: Int => Int): Set = {
    // return a set by applying a tranformation to each element of S
    y => exists(s, x => y == f(x))
  }

  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ", ", "}")
  }

  def printSet(s: Set) {
    println(toString(s))
  }

  // tests
  def main(args: Array[String]) {
    // Singleton sets
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    
    // unions
    val u12 = union(s1, s2)
    val u123 = union(u12, s3)
    val u45 = union(s4, s5)
    val u12345 = union(u123, u45)

    // intersections
    val i45 = intersect(u12345, u45)
    
    // diff
    val d123 = diff(u12345, u45)

    // printing
    printSet(u12345) // 1, 2, 3, 4, 5
    printSet(i45)    // 4, 5
    printSet(d123)   // 1, 2, 3

    val even = filter(u12345, x => x % 2 == 0) // 2, 4
    printSet(even) // 2, 4
    
    println(forall(d123, x => x % 2 == 0)) // false
    println(forall(even, x => x % 2 == 0)) // true
    println(exists(d123, x => x % 2 == 0)) // true
    println(exists(even, x => x % 2 == 1)) // false

    printSet(map(u12345, x => x * x))
  }
}
