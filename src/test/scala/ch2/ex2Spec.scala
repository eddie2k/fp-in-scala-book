package ch2

import scala.Ordering
import org.scalatest.fixture.FunSuite

class ex2Spec extends FunSuite {

  type FixtureParam = ex2

  override def withFixture(test: OneArgTest) = { // Define a shared fixture

    val fixture = new ex2()
    // Shared setup (run at beginning of each test)
    try test(fixture)
    finally {
      // Shared cleanup (run at end of each test)
    }
  }

  test("Null array should throw IllegalArgumentException") { sut =>
    intercept[IllegalArgumentException] {
      sut.isSorted(null, Ordering.Int.lt)
    }
  }

  test("Null comparison function should throw IllegalArgumentException") { sut =>
    intercept[IllegalArgumentException] {
      sut.isSorted(Array.emptyIntArray, null)
    }
  }

  test("Empty array should be sorted") { sut =>
    assert(sut.isSorted(Array.emptyIntArray, Ordering.Int.lt))
  }

  test("Single-element array should be sorted") { sut =>
    assert(sut.isSorted(Array(5), Ordering.Int.lt))
  }

  test("Two-element sorted array should be sorted") { sut =>
    assert(sut.isSorted(Array(1, 2), Ordering.Int.lt))
  }

  test("Two-element non-sorted array should not be sorted") { sut =>
    assert(!sut.isSorted(Array(3, 2), Ordering.Int.lt))
  }

  test("Five-element sorted array should be sorted") { sut =>
    assert(sut.isSorted(Array(1, 2, 3, 4, 5), Ordering.Int.lt))
  }

}