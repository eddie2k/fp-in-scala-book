package ch3

import org.scalatest.fixture.FunSuite

class ex6Spec extends FunSuite {
  type FixtureParam = ex6

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex6()

    try test(fixture)
    finally {

    }
  }

  test("init of the empty list should throw IllegalArgumentException") { sut =>
    intercept[IllegalArgumentException] {
      (new ex6()).init(List())
    }
  }

  test("init of single-element list should return the empty list") { sut =>
    assert((new ex6()).init(List(1)) == Nil)
  }

  test("init of multiple-element list should return the list without the last element") { sut =>
    assert((new ex6()).init(List(1,2,3,4,5)) == List(1,2,3,4))
  }

}