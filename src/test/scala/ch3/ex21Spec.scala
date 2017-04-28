package ch3

import org.scalatest.fixture.FunSuite

class ex21Spec extends FunSuite {

  type FixtureParam = ex21
  override def withFixture(test: OneArgTest) = {

    val fixture = new ex21()

    try test(fixture)
    finally {

    }
  }

  test("filtering the empty list should return the empty list") { sut =>
    def alwaysTrue(x: Any): Boolean = true
    assert(sut.filterUsingFlatMap(Nil)(alwaysTrue) == Nil)
  }

  test("filtering out all the elements should yield the empty list") { sut =>
    def alwaysFalse(x: Any): Boolean = false
    assert(sut.filterUsingFlatMap(List(1, 2, 3))(alwaysFalse) == Nil)
  }

  test("filtering a non-empty list should leave values that pass the condition") { sut =>
    def even(x: Int): Boolean = x % 2 == 0
    assert(sut.filterUsingFlatMap(List(1, 2, 3, 4, 5, 6))(even) == List(2, 4, 6))
  }
}