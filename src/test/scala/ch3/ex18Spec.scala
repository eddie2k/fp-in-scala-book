package ch3

import org.scalatest.fixture.FunSuite

class ex18Spec extends FunSuite {

  type FixtureParam = ex18
  override def withFixture(test: OneArgTest) = {

    val fixture = new ex18()

    try test(fixture)
    finally {

    }
  }

  test("map for an empty list should return the empty list") { sut =>
    def any(x: Any): Any = 0
    assert(sut.map(Nil)(any) == Nil)
  }

  test("map for an on-empty list should apply the function to all the values") { sut =>
    assert(sut.map(List(1, 2, 3))(_ * 2) == List(2, 4, 6))
  }

}