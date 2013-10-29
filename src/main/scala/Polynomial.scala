package org.playwithscala

/* Representing polynomials as maps: 
 x^3 - 2x + 5 => Map(0 -> 5, 1 -> -2, 3 -> 1) { degree -> coefficient }
*/

object Polynomials {
  def main(args: Array[String]) {
    val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
    val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
    println(p1 + p2)
  }

  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0 // default value for all other exp
    override def toString = 
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "

    def + (other: Poly): Poly = new Poly(terms ++ (other.terms map adjust))

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    } 

    // another way to add
    def add (other: Poly) = new Poly((other.terms foldLeft this.terms)(addTerm))
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }
      
  }
}
