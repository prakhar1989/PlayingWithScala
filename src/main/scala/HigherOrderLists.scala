package org.playwithscala

// examples for using higher order functions with lists
object HigherOrderLists {

  def main(args: Array[String]) {
    val l = List(19, 412, 3, 12, 43)
    val s = scaleList(l, 10)
    val data = List("a", "a", "a", "b", "b", "c", "a")
    println(s)
    println(pack(data))
    println(encode(data))
  }

  def scaleList(xs: List[Int], factor: Int): List[Int] = 
    xs map (x => x * factor)

  def squareList(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  def posElems(xs: List[Int]): List[Int] = 
    xs filter (x => x > 0)

  def pack[T](xs: List[T]): List[List[T]] = 
    // packs consecutive duplicates of list elements into sublists
    xs match {
      case Nil => Nil
      case x :: xs1 => 
        val (first, rest) = xs span (y => y == x)
        first :: pack(rest)
    }
  
  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.length))
}

