package org.playwithscala

object Grooving {
  
  def main(args: Array[String]) {
    val names = List("Mike", "Marvin", "Bhagat", "Horacio");
    names.filter(_.length > 4).foreach(println)
  }
}
