package org.playwithscala

import java.io.FileReader
import java.io.FileNotFoundException
import java.io.IOException

object ThrowExp {
  def main(args: Array[String]) {
    try {
      val f = new FileReader("something.txt")
    } catch {
      case ex: FileNotFoundException => // handle missing file
      case ex: IOException => // Handle other IO error
    } finally {
      println("close the file")
      //f.close() // executed no matter what
    }
  }

}
