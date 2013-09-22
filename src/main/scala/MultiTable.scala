package org.playwithscala

object MultiTable {

  def main(args: Array[String]) {
    println(makeTable)
  }

  def makeRowSeq(row: Int) = 
    for (col <- 1 to 10) yield {
      val prod = (row * col).toString
      val padding = " " * (4 - prod.length)
      padding + prod // yield returns a list of this
    }

  def makeRow(row: Int): String = makeRowSeq(row).mkString

  def makeTable(): String = {

    val tableSeq = // a sequence of row strings
      for (row <- 1 to 10)
      yield makeRow(row)

    tableSeq.mkString("\n")
  }
}

