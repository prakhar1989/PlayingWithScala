package org.playwithscala

object ListObjects {
  def main(args: Array[String]) {
    val c = new Cons[Int](30, new Cons(10, new Nil))
    println(c)
  }

  //List(1, 2) = List.apply(1, 2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T](x1: T): List[T] = new Cons(x1, new Nil)
}

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  override def toString: String
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  override def toString = head + ", " + tail.toString
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def toString = "."
}
