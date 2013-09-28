package org.playwithscala

import math.abs

object Fixedpoint {
  
  def main(args: Array[String]) {
    val answer = fixedPoint(x => 1 + x/2)(1) // calling with initial function and initialguess
    // println(answer)
    println(sqrt(16))
  }

  def isCloseEnough(x: Double, y: Double) = 
    abs((x - y) / x) / x < 0.0001

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      println("guess: " + guess + ", next: " + next)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def sqrt(x: Double) = fixedPoint(y => x / y)(1.0)
}
