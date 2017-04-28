package ch3

import org.scalatest.fixture.FunSuite

class ex2Spec extends FunSuite {
  type FixtureParam = ex2

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex2()

    try test(fixture)
    finally {

    }
  }

  test("tail of empty list should throw IllegalArgumentException") { sut =>
    intercept[IllegalArgumentException] {
      (new ex2).tail(Nil)
    }
  }

  test("tail of non-empty list should return its tail") { sut =>
    val l = List(1, 2, 3)
    assert((new ex2).tail(l) == List(2, 3))
  }
}