package org.playwithscala

object HigherFunctions {

  def main(args: Array[String]){
    println(sum(x => x * x, 1, 10))
    println(sum(x => x * x * x, 1, 10))  // calling with anonymous functions
    println(sum(id, 1, 10)) // calling with predefined functions
    
    // currying
    println(sumInts(2, 10) + sumSquares(10, 14))
    println(sumCurry (cube) (1, 5)) // == (sum (cube)) (1, 10)

    // exercises
    println(product(x => x)(2, 5))
    println(factorial(4)) // gives 24

    println(productReduce(x => x)(1, 4)) //gives 24

  }

  def sumInts = sumCurry(x => x)
  def sumSquares = sumCurry(x => x * x)
  def sumCubes = sumCurry(x => x * x * x)

  def cube(x: Int) = x * x * x

  def id(a: Int) = a

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    // tail recursive variant of sum

    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, acc + f(a))

    loop(a, 0)
  }
  
  def sumCurry(f: Int => Int): (Int, Int) => Int = {
    // a function that returns a function
    def sumF(a: Int, b: Int) : Int = 
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    
    sumF
  }
  
  def sumSugar(f: Int => Int)(a: Int, b: Int): Int = 
    // currying with syntactic sugar applied
    if (a > b) 0
    else f(a) + sumSugar(f)(a + 1, b)

  def product(f: Int => Int)(a: Int, b: Int): Int = 
    // currying with syntactic sugar applied
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)

  def factorial(n: Int) = product(x => x)(1, n)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = 
    /*
     * mapReduce takes a function (int to int),
     * combine: that takes 2 ints and returns an int 
     * a zero value (either 1 or 0), 
     * and a and b 
     */
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  def productReduce(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
}
