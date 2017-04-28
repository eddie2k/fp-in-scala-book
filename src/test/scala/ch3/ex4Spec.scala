package ch3

import org.scalatest.FunSuite

class ex4Spec extends FunSuite {

  override def withFixture(test: NoArgTest) = { // Define a shared fixture
    // Shared setup (run at beginning of each test)
    try test()
    finally {
      // Shared cleanup (run at end of each test)
    }
  }

  test("dropWhile for an empty list should return an empty list") {
    assert((new ex4).dropWhile(Nil)(_ => true) == Nil)
  }

  test("dropWhile for a non-empty list with always-false predicate should return same list") {
    val l = List(1, 3, 5, 7)
    assert((new ex4).dropWhile(l)(_ => false) == l)
  }

  test("dropWhile for a non-empty list should drop first matching items") {
    val l = List(1, 3, 5, 6, 8, 7, 9)
    def isOdd: (Int => Boolean) = (a: Int) => ((a % 2) == 1)
    assert((new ex4).dropWhile(l)(isOdd) == List(6, 8, 7, 9))
  } 
}