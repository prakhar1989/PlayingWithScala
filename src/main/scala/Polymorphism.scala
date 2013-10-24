package org.playwithscala.poly

object Polymorphism {

  def main(args: Array[String]) {
    val s1 = singleton[Int](1)
    val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
    println(nth(2, list))
  }

  // a function taking a type parameter
  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  def nth[T](n: Int, xs: List[T]): T = {
    if (n < 0 || xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
    else nth(n - 1, xs.tail)
  }
  
}

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

// in the def below, val is used to set up value parameters
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException
  def tail: Nothing = throw new NoSuchElementException
}

