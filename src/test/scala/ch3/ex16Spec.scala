package ch3

import org.scalatest.fixture.FunSuite

class ex16Spec extends FunSuite {

  type FixtureParam = ex16

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex16()

    try test(fixture)
    finally {

    }
  }

  test("add1 to empty list should return the empty list") { sut =>
    assert(sut.add1(Nil) == Nil)
  }

  test("add1 to non-empty list should return all elements incremented by 1") { sut =>
    assert(sut.add1(List(0, 1, 2, 3)) == List(1, 2, 3, 4))
  }
}