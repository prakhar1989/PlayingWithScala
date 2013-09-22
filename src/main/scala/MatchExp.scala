package org.playwithscala

object MatchExp {
  def main(args: Array[String]) {
    val firstArg = if (args.length > 0) args(0) else ""

    val sides = 
      firstArg match {
        case "salt" => "pepper"
        case "chips" => "salsa"
        case "eggs" => "bacon"
        case _ => "huh?"
      }

    println(firstArg + " go great with " + sides)
  }
}
