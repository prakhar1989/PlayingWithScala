package org.playwithscala

object ReducingLists {

  def main(args: Array[String]) {
    val l = List(10, 412, 42, -12)
    println(sum(l))
    println(sum(l, -100))
    println(product(l))
    // calculate length
    println(lengthFun(l))
    // reimplement map
    println(mapFun(l, ((x: Int) => x * x)))
  }

  def sum(xs: List[Int], init: Int = 0) = 
    (xs foldLeft init) (_ + _) // works for all lists
    // (init :: xs) reduceLeft (_ + _)  doesnt work for empty lists
    // same as (init :: xs) reduceLeft ((x, y) => x + y)
  
  def product(xs: List[Int], init: Int = 1) =
    (xs foldLeft init) (_ * _) 
    //(init :: xs) reduceLeft (_ * _)
    // same as (init :: xs) reduceLeft ((x, y) => x * y)

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((x: T, accList: List[U]) => f(x) :: accList)

  def lengthFun[T](xs: List[T]): Int = 
    (xs foldRight 0)((x: T, acc: Int) => acc + 1)

}

