import org.scalatest.FunSuite

class HelloSuite extends FunSuite {

  import Hello._

  // greater than function
  test("greater than function") {
    assert(greater(2, 4) === 4)
  }

  test("greater than function - zero value") {
    assert(greater(2, 0) === 2)
  }

  // max function
  test("find max for simple list") {
    assert(max(List(2, 10, 19, -12, 42)) === 42)
  }

  test("find max - same element list") {
    assert(max(List(1, 1)) === 1)
  }
}
