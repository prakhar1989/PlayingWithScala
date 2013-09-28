package org.playwithscala
import scala.io.Source

object Functions {

  def main(args: Array[String])  {
     processFile("temp.txt", 10)
     val lines = showLongLines("temp.txt", 10)
     printArray("prakhar", "is", "a", "programmer")
     val friends = Array("Shanu", "Dhanam", "Swati", "Mona")
     printArray(friends: _*)
  }

  def processFile(filename: String, width: Int) {
    val lines = Source.fromFile(filename).getLines
    for (line <- lines) {
      processLine(line)
    }

    def processLine(line: String) {
      if (line.length > width) 
        println(filename + ": " + line.trim)
    }
  }

  def showLongLines(filename: String, width: Int) = {
    val lines = Source.fromFile(filename).getLines
    lines.filter(_.length > width)
  }

  def printArray(args: String*) {
    args.foreach(println)
  }
}
