package ch3

import org.scalatest.fixture.FunSuite

class ex17Spec extends FunSuite {

  type FixtureParam = ex17
  override def withFixture(test: OneArgTest) = {

    val fixture = new ex17()

    try test(fixture)
    finally {

    }
  }

  test("doubleToString for an empty list should return the empty list") { sut =>
    assert(sut.doubleToString(Nil) == Nil)
  }

  test("doubleToString for a non-empty list should return the values converted to string") { sut =>
    assert(sut.doubleToString(List(0d, 2.5d, 100.000d, 123.0000001d)) ==
      List("0.0", "2.5", "100.0", "123.0000001"))
  }
}