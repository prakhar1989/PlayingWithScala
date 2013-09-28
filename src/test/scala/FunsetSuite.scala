import org.scalatest.FunSuite

class FunSetSuite extends FunSuite {

  import org.playwithscala.FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    
    // unions
    val u12 = union(s1, s2)
    val u123 = union(u12, s3)
    val u45 = union(s4, s5)
    val u12345 = union(u123, u45)

    // intersections
    val i45 = intersect(u12345, u45)
    
    // diff
    val d123 = diff(u12345, u45)
  }

  test("singletonSet works") {
    new TestSets {
      assert(contains(s1, 1))
      assert(contains(s2, 2))
      assert(contains(s3, 3))
    }
  }

  test("union contains all elements") {
    new TestSets {
      assert(contains(u12345, 1))
      assert(contains(u12345, 2))
      assert(contains(u12345, 3))
      assert(!contains(u12345, 6))
    }
  }

  test("intersection contains all elements") {
    new TestSets {
      assert(contains(i45, 4))
      assert(contains(i45, 5))
      assert(!contains(i45, 6))

    }
  }

  test("difference contains the right elements") {
    new TestSets {
      assert(contains(d123, 1))
      assert(contains(d123, 2))
      assert(contains(d123, 3))
      assert(!contains(d123, 4))
    }
  }

  test("filter contains the right elements") {
    new TestSets {
      val even = filter(u12345, x => x % 2 == 0)
      assert(contains(even, 2))
      assert(contains(even, 4))
      assert(!contains(even, 1))
    }
  }

  test("forall works as expected") {
    new TestSets {
      val even = filter(u12345, x => x % 2 == 0)
      val odd = filter(u12345, x => x % 2 == 1)
      assert(!forall(u12345, x => x % 2 == 0))
      assert(forall(even, x => x % 2 == 0))
      assert(!forall(odd, x => x % 2 == 0))
    }
  }

  test("exists works as expected") {
    new TestSets {
      val even = filter(u12345, x => x % 2 == 0)
      val odd = filter(u12345, x => x % 2 == 1)
      assert(exists(u12345, x => x % 2 == 0))
      assert(!exists(even, x => x % 2 == 1))
      assert(!exists(odd, x => x % 2 == 0))
      assert(exists(even, x => x % 2 == 0))
      assert(exists(odd, x => x % 2 == 1))
    }
  }

  test("map works as expected") {
    new TestSets {
      val squared = map(u12345, x => x * x)
      assert(contains(squared, 1))
      assert(contains(squared, 4))
      assert(contains(squared, 9))
      assert(contains(squared, 16))
    }
  }
}
