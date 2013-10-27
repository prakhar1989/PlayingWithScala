package org.playwithscala

object Fizzbuzz {
  def main(args: Array[String]) {
    FBIdiomatic
    FB2liner
  }

  def FBIdiomatic = 
    for (x <- 1 to 100) println (
      (x % 3, x % 5) match {
        case (0, 0) => "FizzBuzz"
        case (0, _) => "Fizz"
        case (_, 0) => "Buzz"
        case _      => x
      }
    )

  def FB2liner = {
    def f(a: Int, b: Int, c: String, d: String): String = if (a % b == 0) c else d
    for (i <- 1 to 100) println(f(i, 15, "FizzBuzz", f(i, 3, "Fizz", f(i, 5, "Buzz", i.toString))))
  }

}

