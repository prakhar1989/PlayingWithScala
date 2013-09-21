object RecFun {

  def main(args: Array[String]) {
    println("---------------")
    println("Pascal's Triangle")
    val depth = 4
    for (row <- 0 to depth) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("---------------")
    println("Testing Balance")
    val testString = "(just) an example"
    println(balance(testString.toList))
    println(balance("())(".toList))


    println("---------------")
    println("Counting Change")
    println(countChange(5, List(2, 4, 1)))
  }

  /* Exercise 1 */
  def pascal(c: Int, r: Int): Int = 
    if (c == 0 | r == c) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  

  /* Exercise 2 */
  def balance(chars: List[Char]): Boolean = {

    def counter(parenCounter: Int, chars: List[Char]) : Boolean =
      if (chars.isEmpty || parenCounter < 0) parenCounter == 0
      else if (chars.head == '(') counter(parenCounter + 1, chars.tail)
      else if (chars.head == ')') counter(parenCounter -1, chars.tail)
      else counter(parenCounter, chars.tail)

    counter(0, chars)
  }
    

  /* Exercise 3 */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

}
