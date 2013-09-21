package org.playwithscala

class Rational(n: Int, d: Int)  {
  
  require(d != 0)

  // fields 
  private val g = gcd(n.abs, d.abs) 
  val numer = n / g             // these are automatically reduced as soon as 
  val denom = d / g             // new objects are created.
  
  def this(n: Int) = this(n, 1) // auxiliary constructor

  override def toString = numer + "/" + denom

  def gcd(a: Int, b: Int): Int = 
    if (b == 0) a else gcd(b, a % b)

  def + (that: Rational) : Rational = // add operation
    new Rational(
      numer * that.denom + denom * that.numer, 
      denom * that.denom
    )

  def + (i: Int): Rational = // overloading add
    new Rational( numer + i * denom, denom)

  def * (that: Rational): Rational =  // multiply operation
    new Rational(numer * that.numer, denom * that.denom)

  def * (i: Int): Rational = 
    new Rational( numer * i, denom)

  def - (that: Rational): Rational = // subtraction
    new Rational(
      numer * that.denom - denom * that.numer, 
      denom * that.denom
    )

  def - (i: Int): Rational =
    new Rational( numer - i * denom, denom)

  def / (that: Rational): Rational = // not too hard to guess, is it?
    new Rational( numer * that.denom, denom * that.numer )

  def / (i: Int): Rational = 
    new Rational( numer, denom * i)

  def lessThan(that: Rational) : Boolean = 
    this.numer * that.denom < that.numer * this.denom

  def max(that: Rational): Rational =
    if (this lessThan that) that else this

}
