package ch3

import org.scalatest.fixture.FunSuite

class ex5Spec extends FunSuite {
  type FixtureParam = ex5

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex5()

    try test(fixture)
    finally {

    }
  }

  test("setHead of empty list should throw IllegalArgumentException") { sut =>
    intercept[IllegalArgumentException] {
      val any = 3
      (new ex5).setHead(Nil, any)
    }
  }

  test("setHead in a non-empty list should return the updated list") { sut =>
    assert(new (ex5).setHead(List(1, 2, 3), 100) == List(100, 2, 3))
  }

}