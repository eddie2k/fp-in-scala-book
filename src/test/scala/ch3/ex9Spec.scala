package ch3

import org.scalatest.fixture.FunSuite

class ex9Spec extends FunSuite {

    type FixtureParam = ex9

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex9()

    try test(fixture)
    finally {

    }
  }

  test("length of empty list should be 0") { sut =>
    assert((new ex9).length(Nil) == 0)
  }

  test("lenght of non-empty list should match number of elements") { sut =>
    assert((new ex9()).length(List(1, 2, 3, 4, 5)) == 5)
  }
}