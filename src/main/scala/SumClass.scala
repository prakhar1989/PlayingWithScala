package org.playwithscala

object SumClass {

  def main(args: Array[String]) {
    val e1 = new Sum(new Number(10), new Number(3))
    e1.print; println(" = " + e1.eval + "\n")
  }

}

abstract class Expr {
  def eval: Int
  def print
}

class Number(n: Int) extends Expr {
  def eval: Int = n
  def print { Console.print(n) }
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  def eval: Int = e1.eval + e2.eval
  def print {
    Console.print("(")
    e1.print
    Console.print("+")
    e2.print
    Console.print(")")
  }
}

class Product(e1: Expr, e2: Expr) extends Expr {
  def eval: Int = e1.eval * e2.eval
  def print {
    Console.print("(")
    e1.print
    Console.print("x")
    e2.print
    Console.print(")")
  }
}
