package ch3

import org.scalatest.fixture.FunSuite

class ex23Spec extends FunSuite {

  type FixtureParam = ex23

  override def withFixture(test: OneArgTest) = {
    val fixture = new ex23()

    try test(fixture)
    finally {

    }
  }

  test("zipWith of two empty list should return the empty list") { sut =>
    def any(x: Any, y: Any): Any = AnyRef
    assert(sut.zipWith(Nil, Nil)(any(_, _)) == Nil)
  }

  test("zipWith of non-empty and empty list should return the empty list") { sut =>
    def any(x: Any, y: Any): Any = AnyRef
    assert(sut.zipWith(List(1, 2, 3), Nil)(any) == Nil)
  }

  test("zipWith of empty and non-empty list should return the empty list") { sut =>
    def any(x: Any, y: Any): Any = AnyRef
    assert(sut.zipWith(Nil, List(1, 2, 3))(any) == Nil)
  }

  test("zipWith two non-empty list of the same size should apply the function to all the pairs of both lists") { sut =>
    def sumIntAndLenght(x: Int, s: String): Int = x + s.length()
    assert(sut.zipWith(List(1, 2, 3), List("1", "22", "333"))(sumIntAndLenght) == List(2, 4, 6))
  }

  test("zipWith two non-empty list of different size should apply the function only to pairs present in both lists") { sut =>
    def sumIntAndLenght(x: Int, s: String): Int = x + s.length()
    assert(sut.zipWith(List(1, 2, 3, 4, 5), List("1", "22", "333"))(sumIntAndLenght) == List(2, 4, 6))
  }

}