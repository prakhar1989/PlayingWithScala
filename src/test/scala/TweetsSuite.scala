package org.playwithscala.tweets
import org.scalatest.FunSuite

class TweetSetSuite extends FunSuite {

  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20)) // a, b
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 40)
    val set4c = set3.incl(c) // a, b, c
    val set4d = set3.incl(d) // a, b, d
    val set5 = set4c.incl(d) // a, b, c, d
    val e = new Tweet("e", "e body", 2)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("contains and incl santity") {
    new TestSets {
      assert(set5.contains(d))
      assert(set5.contains(c))
      assert(!set5.contains(e))
    }
  }

  test("check size") {
    new TestSets {
      assert(size(set5) === 4)
    }
  }

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("mostRetweeted: with empty set") {
    new TestSets {
      intercept[java.util.NoSuchElementException] {
        val tw = set1.mostRetweeted
      }
    }
  }

  test("mostRetweeted: with set5c") {
    new TestSets {
      val tw = set5.mostRetweeted
      assert(tw.user === "d")
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "d")
    }
  }

}

