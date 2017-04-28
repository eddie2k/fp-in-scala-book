package ch3

import org.scalatest.fixture.FunSuite

class ex3Spec extends FunSuite {
  type FixtureParam = ex3

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex3()

    try test(fixture)
    finally {

    }
  }

  test("drop a negative number of elements should throw IllegalArgumentException") { sut =>
    intercept[IllegalArgumentException] {
      (new ex3).drop(List(1, 2, 3), -1)
    }
  }

  test("drop as many elements as list size should return the empty list") { sut =>
    assert((new ex3).drop(List(1, 2, 3), 3) == Nil)
  }

  test("drop a number of elements n should yield a list without the first n elements") { sut =>
    assert((new ex3).drop(List(1, 2, 3, 4, 5), 2) == List(3, 4, 5))
  }
}