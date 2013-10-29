package org.playwithscala
import scala.io.Source

object Mnemonics {

  def main(args: Array[String]) {

    val in = Source.fromFile("../words.txt")
    val all_words = in.getLines.toList
    val words = all_words filter (word => word forall (chr => chr.isLetter))

    val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
                   '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

    val charCode: Map[Char, Char] = 
      for { 
        (digit, str) <- mnem
        letter <- str 
      } yield letter -> digit
    
    def wordCode(word: String): String = word.toUpperCase map charCode

    val wordsForNum: Map[String, Seq[String]] = 
      words groupBy wordCode withDefaultValue Seq()

    def encode(number: String): Set[List[String]] = 
      // takes a number and returns a list of valid english words
      if (number.isEmpty) Set(List())
      else {
        for {
         split <- 1 to number.length
         word <- wordsForNum(number take split)
         rest <- encode(number drop split)
        } yield word :: rest
      }.toSet


    def translate(number: String): Set[String] = 
      encode(number) map (_ mkString " ")

    println(translate("7225247386")) // contains "Scala is fun" 9810013059
    
  }
}
