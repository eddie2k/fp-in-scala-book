package ch3

import org.scalatest.fixture.FunSuite

class ex13Spec extends FunSuite {

  type FixtureParam = ex13

  override def withFixture(test: OneArgTest) = {
    val fixture = new ex13()

    try test(fixture)
    finally {}
  }

  test("foldLeft in the empty list should return the neutral element") { sut =>
    val any: (Int, Int) => Int = (x: Int, acc: Int) => x
    assert(sut.foldLeft(Nil, 123)(any) == 123)
  }

  test("foldLeft in non-empty list should apply operation from the left") { sut =>
    def concat = (a: String, b: String) => a + b
    assert(sut.foldLeft(List("Hello", "world"), "Goodbye")(concat) == "GoodbyeHelloworld")
  }

  test("foldRight in the empty list should return the neutral element") { sut =>
    val any: (Int, Int) => Int = (x: Int, acc: Int) => x
    assert(sut.foldRight(Nil, 123)(any) == 123)
  }

  test("foldRight in non-empty list should apply operation from the right") { sut =>
    def concat = (a: String, b: String) => a + b
    assert(sut.foldRight(List("Hello", "world"), "Goodbye")(concat) == "HelloworldGoodbye")
  }
}