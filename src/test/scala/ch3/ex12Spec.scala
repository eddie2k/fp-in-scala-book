package ch3

import org.scalatest._

class ex12Spec extends fixture.FunSuite {

  type FixtureParam = ex12 // Define the type of the passed fixture object

  override def withFixture(test: OneArgTest) = { // Define a shared fixture
    // Shared setup (run at beginning of each test)
    val fixture = new ex12()

    try test(fixture)
    finally {
      // Shared cleanup (run at end of each test)
    }
  }

  test("reverse of empty list should be the empty list") { sut =>
    assert(sut.reverse(Nil) == Nil)
  }

  test("reverse of non-empty list should return elements in reverse order") { sut =>
    assert(sut.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

}