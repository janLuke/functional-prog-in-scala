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
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


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
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  /**
    * My Tests
    */

  def rangeSet(start: Int, end: Int): Set =
    (elem: Int) => (start <= elem && elem <= end)

  def listToSet(elems: List[Int]): Set = {
    if (elems.size == 1) singletonSet(elems.head)
    else union(singletonSet(elems.head), listToSet(elems.tail))
  }

  def checkSet(s: Set, in: List[Int], out: List[Int]): Unit = {
    in.foreach(elem => assert(s(elem), s"$elem should be in the set but it isn't"))
    out.foreach(elem => assert(!s(elem), s"$elem should NOT be in the set but it is"))
  }

  trait DisjointFixture {
    val s1346 = listToSet(List(1, 3, 4, 6))
    val s257 = listToSet(List(2, 5, 7))
  }

  trait IncludedFixture {
    val s1346 = listToSet(List(1, 3, 4, 6))
    val s14 = listToSet(List(1, 4))
  }

  trait IntersectedFixture {
    val s1346 = listToSet(List(1, 3, 4, 6))
    val s1478 = listToSet(List(1, 4, 7, 8))
  }

  /**
    * Intersection
    */
  test("intersection between disjoint sets") {
    new DisjointFixture {
      val s = intersect(s1346, s257)
      checkSet(s, List(), List(1, 2, 3, 4, 5, 6, 7))
    }
  }

  test("intersection with some elements in common") {
    new IntersectedFixture {
      val s = intersect(s1346, s1478)
      checkSet(s, List(1, 4), List(2, 3, 5, 6, 7, 8, 9))
    }
  }

  test("intersection with one set included in the other") {
    new IncludedFixture {
      val s = intersect(s1346, s14)
      checkSet(s, List(1, 4), List(2, 3, 5, 6, 7, 8))
    }
  }

  /**
    * Union
    */
  test("union with some elements in common") {
    new IntersectedFixture {
      val s = union(s1346, s1478)
      checkSet(s, List(1, 3, 4, 6, 7, 8), List(0, 2, 5, 9))
    }
  }

  test("union with one set included in the other") {
    new IncludedFixture {
      val s = union(s1346, s14)
      checkSet(s, List(1, 3, 4, 6), List(2, 5, 7))
    }
  }

  /**
    * Difference
    */
  test("difference between disjoint sets") {
    new DisjointFixture {
      val s = diff(s1346, s257)
      checkSet(s, List(1, 3, 4, 6), List(0, 2, 5, 7))
    }
  }

  test("difference with some elements in common") {
    new IntersectedFixture {
      val s = diff(s1346, s1478)
      checkSet(s, List(3, 6), List(1, 4, 5, 7, 8, 9))
    }
  }

  test("difference with empty result") {
    new IncludedFixture {
      val s = diff(s14, s1346)
      checkSet(s, List(), List(1, 2, 3, 4, 5, 6, 7, 8))
    }
  }

  /**
    * Filter
    */
  test("filter even and odd number") {
    val s: Set = rangeSet(0, 5)
    val evenSet = filter(s, x => (x % 2 == 0))
    val oddSet = filter(s, x => (x % 2 != 0))
    val odd = List(1, 3, 5)
    val even = List(0, 2, 4)
    checkSet(evenSet, even, odd)
    checkSet(oddSet, odd, even)
  }

  test("filter with empty result") {
    val oddNums = List(1, 3, 5)
    val s = listToSet(oddNums)
    val empty = filter(s, x => (x % 2 == 0))
    checkSet(empty, List(), List(0, 1, 2, 3, 4, 5, 6))
  }

  /**
    * For all
    */
  test("forall true") {
    val s = listToSet(List(1, 3, 5))
    assert(forall(s, x => x % 2 == 1))

    val t = rangeSet(-10, -1)
    assert(forall(t, x => x < 0))
  }

  test("forall false") {
    val s = listToSet(List(1, 3, 5, 6))
    assert(!forall(s, x => x % 2 == 1), "6 is not odd")

    val t = rangeSet(-10, 0)
    assert(!forall(t, x => x < 0), "0 is not negative")
  }

  /**
    * Exists
    */
  test("exists true") {
    val s = listToSet(List(1, 3, 4, 7))
    assert(exists(s, x => x % 2 == 0), "4 is even")

    val t = listToSet(List(1, 3, 7, -4))
    assert(exists(t, x => x < 0), "-4 is negative")
  }

  test("exists false") {
    val s = listToSet(List(1, 3, 5, 7))
    assert(!exists(s, x => x % 2 == 0), "no even numbers")

    val t = listToSet(List(1, 3, 4, 7))
    assert(!exists(t, x => x < 0), "no negative numbers")
  }

  /**
    * Map
    */
  test("test map") {
    val s = listToSet(List(1, 3, 6, 10))
    val doubles = map(s, x => 2*x)
    checkSet(doubles, List(2, 6, 12, 20), List(1, 3, 4, 10, 11))
  }
}
