package org.playwithscala

object Querying {
  def main(args: Array[String]) {

    val books: Set[Book] = Set(
      Book(title="SICP", authors=List("Abelson", "Sussman")),
      Book(title="Intro to FP", authors=List("Richard", "Phil")),
      Book(title="Effective Java", authors=List("Joshua", "Bloch")),
      Book(title="Java Puzzlers", authors=List("Near", "Joshua")),
      Book(title="Programming in Scala", authors=List("Venners", "Odersky")),
      Book(title="Python for data analysis", authors=List("Joshua", "Wes"))
    )

  val xs = for (b <- books; a <- b.authors if a startsWith "Abel") yield b.title
  val ys = for (b <- books if b.title.indexOf("Java") >= 0) yield b.title

  // author who's written atleast two books
  val authors = for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title // to prevent duplicates over all pairs (b1, b2)
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1

  println(authors)
  }

  case class Book(title: String, authors: List[String])
}

