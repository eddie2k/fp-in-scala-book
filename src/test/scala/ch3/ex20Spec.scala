package ch3

import org.scalatest.fixture.FunSuite

class ex20Spec extends FunSuite {

  type FixtureParam = ex20
  override def withFixture(test: OneArgTest) = {

    val fixture = new ex20()

    try test(fixture)
    finally {

    }
  }

  test("flatMap for the empty list should return the empty list") { sut =>
    def any[A](x: A): List[A] = Nil
    assert(sut.flatMap(Nil)(any) == Nil)
  }

  test("flatMap for a non-empty list should apply the function to each element and concat the results") { sut =>
    def repeatElement[A](x: A): List[A] = List(x, x)
    assert(sut.flatMap(List(1, 2, 3))(repeatElement) == List(1, 1, 2, 2, 3, 3))
  }

}