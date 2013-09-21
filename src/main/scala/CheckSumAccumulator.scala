import scala.collection.mutable.Map

class CheckSumAccumulator {
  private var sum = 0

  def add(b: Byte) { sum += b }  // use curly braces to denote side-effects
  def checksum(): Int = ~(sum & 0xFF) + 1
}

object CheckSumAccumulator {
  private val cache = Map[String, Int]()

  def calculate(s: String): Int = 
    if (cache.contains(s)) 
      cache(s)
    else {
      val acc = new CheckSumAccumulator
      for (c <- s)
        acc.add(c.toByte)
      val cs = acc.checksum()
      cache += (s -> cs)
      cs
    }
}
