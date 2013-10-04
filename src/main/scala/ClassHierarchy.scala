package org.playwithscala

object ClassHierarchy {
  // this is also a singleton object
  def main(args: Array[String]) {
    val t1 = new NonEmpty(3, Empty, Empty)
    val t2 = t1 incl 4 // this doesnt change t1. Returns a new tree instead
    println(t1)
    println(t2)
  }
}

abstract class IntSet { // cannot make objects of this class
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(other: IntSet): IntSet
}

// making sets with binary trees
object Empty extends IntSet { 
  // defines a singleton object
  override def toString = "."
  
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def toString = "{" + left + elem + right + "}"

  def contains(x: Int): Boolean = 
    if (x > elem) right contains x
    else if (x < elem) left contains x
    else true

  def incl(x: Int): IntSet = 
    if (x > elem) new NonEmpty(elem, left, right incl x)
    else if (x < elem) new NonEmpty(elem, left incl x, right)
    else this

  def union(other: IntSet): IntSet = 
    ((left union right) union other) incl elem
}

