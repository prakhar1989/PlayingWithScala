package org.playwithscala

object Lists {
  def main(args: Array[String]) {
    val numbers = 54 :: 21 :: 12 :: Nil
    val more_numbers = 78 :: 100 :: 143 :: Nil
    println(isort(numbers))
    println(append(numbers, more_numbers)) // prints concatenated list

    // function currying
    val intSort = msort((x: Int, y: Int) => x < y) _
    val reverseIntSort = msort((x: Int, y: Int) => x > y) _

    // testing
    println(intSort(numbers))
    println(reverseIntSort(numbers))
  }

  /* insertion sort without pattern matching
  def isort(xs: List[Int]): List[Int] = 
    if (xs.isEmpty) Nil
    else insert(xs.head, isort(xs.tail))

  def insert(x: Int, xs: List[Int]): List[Int] =
    // inserts x in sorted list 
    if (xs.isEmpty || x <= xs.head) x :: xs
    else xs.head :: insert(x, xs.tail)
  */

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x < y) x :: xs
                    else y :: insert(x, ys)
  }

  def append[T](xs: List[T], ys: List[T]): List[T] = 
    xs match { 
      // Appends the reverse of xs to ys
      case List() => ys
      case x :: xs1 => append(xs1, x :: ys)
    }

  def rev[T](xs: List[T]): List[T] =
    xs match { // list reverse O(n*2)
      case List() => List()
      case y :: ys => rev(ys) ::: List(y)
    }

  def last[T](xs: List[T]): T = 
    xs match { // last element of the list
      case List() => throw new Error("last of empty set")
      case List(x) => x
      case y :: ys => last(ys)
    }
    
  def init[T](xs: List[T]): List[T] =
    xs match { // same as list.init
      case List() => throw new Error("init of empty list")
      case List(x) => List()
      case y :: ys  => y :: init(ys)
    }

  def concat[T](xs: List[T], ys: List[T]): List[T] = 
    // Appends xs into ys (check append above for difference)
    xs match {
      case List() => ys
      case z :: zs => z :: concat(zs, ys)
    }

  def removeAt[T](xs: List[T], n: Int): List[T] =
    (xs take n) ::: (xs drop n + 1)

  def msort[T](less: (T, T) => Boolean)(xs: List[T]): List[T] = {
    // less is the function that defines an ordering on the 
    // parent list. It compares two elements and returns a boolean

    // define the merge function
    def merge(xs: List[T], ys: List[T]): List[T] = 
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) => 
          if (less(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

    val n = xs.length / 2
    
    if (n == 0) xs   // if the list is empty return list
    else {           // split lists and then merge them 
      val (ys, zs) = xs splitAt n
      merge(msort(less)(ys), msort(less)(zs))
    }
  }

}

