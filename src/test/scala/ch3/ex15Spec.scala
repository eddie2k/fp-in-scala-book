package ch3

import org.scalatest.fixture.FunSuite

class ex15Spec extends FunSuite {

  type FixtureParam = ex15

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex15()

    try test(fixture)
    finally {

    }
  }

  test("concat an a single empty list should return an empty list") { sut =>
    assert(sut.concat(Nil) == Nil)
  }

  test("concat few empty lists should return an empty list") { sut =>
    assert(sut.concat(List(Nil, Nil, Nil)) == Nil)
  }

  test("concat empty and non-empty list should return all non-emptyelements") { sut =>
    assert(sut.concat(List(List(1, 2), List(3), Nil, List(4, 5), Nil)) == List(1, 2, 3, 4, 5))
  }
}