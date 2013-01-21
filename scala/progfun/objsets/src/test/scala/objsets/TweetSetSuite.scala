package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)

    val t1 = new Tweet("t1", "t1 body", 10)
    val t2 = new Tweet("t2", "t2 body", 2)
    val t3 = new Tweet("t3", "t3 body", 30)
    val t4 = new Tweet("t4", "t4 body", 4)
    val t5 = new Tweet("t5", "t5 body", 30)
    val t6 = new Tweet("t6", "t6 body", 30)
    val t7 = new Tweet("t7", "t7 body", 7)
    val t8 = new Tweet("t8", "t8 body", 8)
    val t9 = new Tweet("t9", "t9 body", 100)
    val set6 = set1.incl(t5).incl(t3).incl(t7).incl(t2).incl(t4).incl(t6).incl(t8)
  }

  def size(set: TweetSet): Int = {
    if (set.isEmpty) 0
    else 1 + size(set.tail)
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

  test("filter: all") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets > 0)) === 4)
    }
  }

  test("filter: none") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets < 0)) === 0)
    }
  }

  test("filter: interesting tree") {
    new TestSets {
      assert(size(set6.filter(tw => tw.retweets == 2)) === 1)
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

  test("ascending: set5") {
    new TestSets {
      println
      val trends = set5.ascendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user === "c")
    }
  }
}
