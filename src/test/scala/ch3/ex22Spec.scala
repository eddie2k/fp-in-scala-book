package ch3

import org.scalatest.fixture.FunSuite

class ex22Spec extends FunSuite {

  type FixtureParam = ex22

  override def withFixture(test: OneArgTest) = {
    val fixture = new ex22()

    try test(fixture)
    finally {

    }
  }

  test("addPairWise of two empty list should return the empty list") { sut =>
    assert(sut.addPairwise(Nil, Nil) == Nil)
  }

  test("addPairWise of non-empty and empty list should return the empty list") { sut =>
    assert(sut.addPairwise(List(1, 2, 3), Nil) == Nil)
  }

  test("addPairWise of empty and non-empty list should return the empty list") { sut =>
    assert(sut.addPairwise(Nil, List(1, 2, 3)) == Nil)
  }

  test("addPairWise two non-empty list of the same size should sum all the pairs of both lists") { sut =>
    assert(sut.addPairwise(List(1, 2, 3), List(11, 12, 13)) == List(12, 14, 16))
  }

  test("addPairWise two non-empty list of different size should sum only pairs present in both lists") { sut =>
    assert(sut.addPairwise(List(1, 2, 3, 4, 5), List(11, 12, 13)) == List(12, 14, 16))
  }

}