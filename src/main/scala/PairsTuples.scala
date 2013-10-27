package org.playwithscala
import math.Ordering

object PairsTuples {

  def main(args: Array[String]) {
    println("Testing mergesort with lists of arbitrary type")

    val nums = List(5, 12, -14, 102, 94)
    val fruits = List("apple", "banana", "pineapple", "pear")

    // explicit
    println("\nExplicit ordering: ")
    println(msort(nums)(Ordering.Int))
    println(msort(fruits)(Ordering.String))
    
    //implicit
    println("\nImplicit ordering: ")
    println(msort(nums))
    println(msort(fruits))
  }

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {

    def merge(xs: List[T], ys: List[T]): List[T] = 
      (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xs1, y :: ys1) => 
          // if x is smaller
          if (ord.lt(x,y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(msort(ys), msort(zs))
    }
  }

}

