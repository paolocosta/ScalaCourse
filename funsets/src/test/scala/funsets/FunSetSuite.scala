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
  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("even is correct") {
    new TestSets {

      assert(!contains(even, 1), "Even 1")
      assert(contains(even, 2), "Even 2")
      assert(!contains(even, 3), "Even 3")
    }
  }

  test("odd is correct") {
    new TestSets {

      assert(contains(odd, 1), "Odd 1")
      assert(!contains(odd, 2), "Odd 2")
      assert(contains(odd, 3), "Odd 3")
    }
  }

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
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
    }
  }

  test("intersect contains only elements present in all sets") {
    new TestSets {
      val s12 = union(s1,s2)
      val s = intersect(s12, s1)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff contains only elements present in the first set and not in the second") {
    new TestSets {
      val s12 = union(s1,s2)
      val s = diff(s12, s1)
      assert(!contains(s, 1), "Diff 1")
      assert(contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter contains all the elements of `s` for which `p` holds") {
    new TestSets {
      val s123 = union(union(s1,s2), s3)
      val s = filter(s123, x => x % 2 == 0)
      assert(!contains(s, 1), "Filter 1")
      assert(contains(s, 2), "Filter  2")
      assert(!contains(s, 3), "Filter  3")
    }
  }

  test("forall") {
    new TestSets {

      assert(forall(all, x => x > -2000), "forall 1")
      assert(!forall(all, x => x > 0), "forall 2")
      assert(forall(even, x => x % 2 ==0), "forall 3")
      assert(!forall(even, x => x % 2 ==1), "forall 3")
      assert(forall(none, x=> false), "forall 4")
      assert(forall(none, x=> true), "forall 4")
      assert(forall(all, x=> true), "forall 5")
      assert(forall(all, x=> true), "forall 6")
      assert(!forall(all, x=> false), "forall 6")

    }
  }

  test("exists") {
    new TestSets {

      assert(exists(all, x => x > 0), "exists 1")
      assert(!exists(all, x => x < -2000), "exists 2")
      assert(exists(even, x => x == 4), "exists 3")
      assert(!exists(even, x => x == 5), "exists 4")
      assert(!exists(none, x=> true), "exists 5")
    }
  }

  test("map") {
    new TestSets {
      val squares = map(all, x=> x*x)
      assert(contains(squares, 1), "map 1")
      assert(contains(squares, 4), "map 2")
      assert(!contains(squares, 3), "map 3")
      assert(contains(squares, 9), "map 4")
      assert(contains(squares, 16), "map 5")
      assert(!contains(squares, 32), "map 6")
    }
  }

}
