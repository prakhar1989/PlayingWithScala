package org.playwithscala

object CollectionScala {
  def main(args: Array[String]) {
    println("\nList of all prime pairs: ")
    println(getPrimePairs(7))
    
    // kicking ass with for expressions 
    println(getForexPrime(7))
  }

  def isPrime(n: Int): Boolean = 
    (2 until n) forall (i => (n % i) != 0)

  def getPairs(n: Int): Seq[(Int, Int)] = 
    (1 until n) flatMap (i => (1 until i) map (j => (i, j)))

  def getPrimePairs(n: Int): Seq[(Int, Int)] = 
    getPairs(n) filter { case(x, y) => isPrime(x + y) }

  def getForexPrime(n: Int): Seq[(Int, Int)] =
    // same function with for comprehensions
    //for (i <- 1 until n; j <- 1 until i if isPrime(i + j)) yield (i, j)
    // more readable below
    for {
      i <- 1 until n
      j <- 1 until i
      if isPrime(i + j)
    } yield (i, j)

  def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for ((x, y) <- xs zip ys) yield x * y).sum
}

