import CheckSumAccumulator.calculate

object Summer extends Application {

  List("fall", "winter", "spring").foreach(
    season => println(season + ": " + calculate(season))
  )

}
