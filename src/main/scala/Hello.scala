package org.playwithscala

object Hello {
  def main(args: Array[String]) =
    println(max(List(4, 2, -2)))

  def greater(a: Int, b: Int) = if (a >= b) a else b
  
  def max(xs: List[Int]): Int = {

    def maxIter(temp: Int, xs: List[Int]): Int = {
      if (xs.isEmpty) temp
      else maxIter(greater(temp, xs.head), xs.tail)
    }

    maxIter(xs.head, xs.tail)
  }
}
