import scala.io.Source

object Files {
  
  def main(args: Array[String]) {

    if (args.length > 0) {
      for (line <- Source.fromFile(args(0)).getLines)
        print(line.length + " - " + line)
    }
    else {
      Console.err.println("please enter a filename")
    }
  }

    
}
