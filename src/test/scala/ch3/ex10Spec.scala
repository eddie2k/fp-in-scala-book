package ch3

import org.scalatest.fixture.FunSuite

class ex10Spec extends FunSuite {

  type FixtureParam = ex10

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex10()

    try test(fixture)
    finally {

    }
  }

  test("fold the empty list should return the neutral element") { sut =>
    val any: (Int, Int) => Int = (x: Int, acc: Int) => x
    assert((new ex10()).foldLeft(Nil, 123)(any) == 123)
  }

  test("fold non-empty list should apply operation from the left") { sut =>
    def concat = (a: String, b: String) => a + b
    assert((new ex10()).foldLeft(List("Hello", "world"), "Goodbye")(concat) == "GoodbyeHelloworld")
  }
}