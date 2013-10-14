package org.playwithscala

// Implmenting natural numbers from first principles - no primitive types 
abstract class Natural {
  def isZero: Boolean
  def predecessor: Natural
  def successor: Natural = new Succ(this)
  def + (that: Natural): Natural
  def - (that: Natural): Natural
}

object Zero extends Natural {
  def isZero: Boolean = true
  def predecessor: Nothing = throw new Error("0.predecessor")
  def + (that: Natural): Natural = that 
  def - (that: Natural): Natural = if (that.isZero) this else throw new Error("0.-")
}

class Succ(n: Natural) extends Natural {
  /* All numbers are represented as succ of the Zero object since there are no primitives.
     1 = Succ(Zero)
     2 = Succ(Succ(Zero)) 
     3 = Succ(Succ(Succ(Zero)))
   */
  def isZero: Boolean = false
  def predecessor: Natural = n // simple from the above examples

  // Succ(Succ(Zero)) + Succ(Succ(Zero))
  // Succ(Succ(Zero) + Succ(Succ(Zero)))
  // Succ(Succ(Zero + Succ(Succ(Zero))))
  // Succ(Succ(Succ(Succ(Zero))))
  def + (that: Natural): Natural = new Succ(n + that)

  // Succ(Succ(Zero)) - Succ(Zero)) === Succ(Zero) 
  // Succ(Zero)) - Zero
  // Succ(Zero)
  def - (that: Natural): Natural = if (that.isZero) this else n - that.predecessor

}
