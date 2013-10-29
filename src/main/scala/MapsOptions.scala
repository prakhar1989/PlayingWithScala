package org.playwithscala

object MapsOptions {
  def main(args: Array[String]) {
    val capitals = Map("US" -> "Washington", "India" -> "New Delhi")
    println(showCapital("India", capitals))
    println(showCapital("UK", capitals))

    val fruits = List("apple", "orange", "pear", "aka", "pineapple", "mango", "olo")
    println(fruits sortWith (_.length < _.length )) // custom sorting
    println(fruits sorted) // lexicographic ordering
    
    // using groupBy
    println(fruits groupBy (_.length))
  }

  def showCapital(country: String, capitals: Map[String, String]) = capitals.get(country) match {
    case Some(capital) => capital
    case None => "missing data"
  }

}
