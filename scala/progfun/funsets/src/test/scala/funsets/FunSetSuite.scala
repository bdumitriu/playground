package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singleton sets work properly") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton 1")
      assert(!contains(s1, 2), "Singleton 1")
      assert(contains(s2, 2), "Singleton 2")
      assert(!contains(s2, 0), "Singleton 2")
      assert(contains(s3, 3), "Singleton 3")
      assert(!contains(s3, -1), "Singleton 3")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("two level union") {
    new TestSets {
      val s  = union(s1, union(s2, s3))
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(contains(s, 3), "Union 3")
      assert(!contains(s, 4), "Union 4")
    }
  }

  test("empty intersection") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersection 1")
      assert(!contains(s, 2), "Intersection 2")
      assert(!contains(s, 3), "Intersection 3")
    }
  }

  test("non-empty intersection") {
    new TestSets {
      val s = intersect(union(union(s1, s2), s3), union(s2, s3))
      assert(!contains(s, 1), "Intersection 1")
      assert(contains(s, 2), "Intersection 2")
      assert(contains(s, 3), "Intersection 3")
    }
  }

  test("empty diff") {
    new TestSets {
      val s = diff(s1, s1)
      assert(!contains(s, 1), "Difference 1")
      assert(!contains(s, 2), "Difference 2")
    }
  }

  test("non-empty diff") {
    new TestSets {
      val s = diff(union(union(s1, s2), s3), union(s2, s3))
      assert(contains(s, 1), "Difference 1")
      assert(!contains(s, 2), "Difference 2")
      assert(!contains(s, 3), "Difference 3")
    }
  }

  test("odd filter") {
    new TestSets {
      val s = filter(union(s1, union(s2, s3)), _ % 2 != 0)
      assert(contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(contains(s, 3), "Filter 3")
      assert(!contains(s, 5), "Filter 5")
    }
  }

  test("false filter") {
    new TestSets {
      val s = filter(union(s1, union(s2, s3)), _ => false)
      assert(!contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(!contains(s, 3), "Filter 3")
      assert(!contains(s, 5), "Filter 5")
    }
  }

  test("true filter") {
    new TestSets {
      val s = filter(union(s1, union(s2, s3)), _ => true)
      assert(contains(s, 1), "Filter 1")
      assert(contains(s, 2), "Filter 2")
      assert(contains(s, 3), "Filter 3")
      assert(!contains(s, 5), "Filter 5")
    }
  }

  test("forall no elements") {
    new TestSets {
      assert(forall(intersect(s1, s2), _ => true))
    }
  }

  test("forall all true") {
    new TestSets {
      assert(forall(union(s1, union(s2, s3)), _ < 5))
    }
  }

  test("forall one false") {
    new TestSets {
      assert(!forall(union(s1, union(s2, s3)), _ != 2))
    }
  }

  test("forall all false") {
    new TestSets {
      assert(!forall(union(s1, union(s2, s3)), _ == 5))
    }
  }

  test("exists no elements") {
    new TestSets {
      assert(!exists(intersect(s1, s2), _ => true))
    }
  }

  test("exists all true") {
    new TestSets {
      assert(exists(union(s1, union(s2, s3)), _ < 5))
    }
  }

  test("exists one true") {
    new TestSets {
      assert(exists(union(s1, union(s2, s3)), _ == 3))
    }
  }

  test("exists all false") {
    new TestSets {
      assert(!exists(union(s1, union(s2, s3)), _ == 5))
    }
  }

  test("map no elements") {
    new TestSets {
      val s = map(intersect(s1, s2), x => x)
      assert(!contains(s, 1), "Map 1")
      assert(!contains(s, 2), "Map 2")
      assert(!contains(s, 3), "Map 3")
    }
  }

  test("map id function") {
    new TestSets {
      val s = map(union(s1, s2), x => x)
      assert(contains(s, 1), "Map 1")
      assert(contains(s, 2), "Map 2")
      assert(!contains(s, 3), "Map 3")
    }
  }

  test("map inc function") {
    new TestSets {
      val s = map(union(s1, s2), _ + 1)
      assert(!contains(s, 1), "Map 1")
      assert(contains(s, 2), "Map 2")
      assert(contains(s, 3), "Map 3")
    }
  }

  test("map non bijective function") {
    new TestSets {
      val s = map(union(s1, s2), _ => 10)
      assert(!contains(s, 1), "Map 1")
      assert(!contains(s, 2), "Map 2")
      assert(!contains(s, 3), "Map 3")
      assert(contains(s, 10), "Map 10")
    }
  }
}
