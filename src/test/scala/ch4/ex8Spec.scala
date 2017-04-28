package ch4

import org.scalatest.fixture.FunSuite

class ex8Spec extends FunSuite {

  type FixtureParam = ex8

  override def withFixture(test: OneArgTest) = {
    val fixture = new ex8()

    try test(fixture)
    finally {

    }
  }

  test("sequence of the empty list should be Right(Nil)") { sut =>
    assert(sut.sequence(Nil) == Right(Nil))
  }

  test("sequence of list of Right should be Right with all elements in the list") { sut =>
    assert(sut.sequence[String, Int](List(Right(1), Right(2), Right(3))) == Right(List(1, 2, 3)))
  }

  test("sequence of list that contains at least one Left should be Left with the first Left on the list") { sut =>
    assert(sut.sequence[String, Int](List(Right(1), Right(2), Left("Invalid!"), Right(3))) == Left("Invalid!"))
  }

  test("traverse of the empty list with any function should be Right(Nil)") { sut =>
    val anyFunction: Any => Either[Any, Any] = _ => null
    assert(sut.traverse[Any, Any, Any](Nil)(anyFunction) == Right(Nil))
  }

  test("traverse of a non-empty list that contains at least one Left should return the first Left") { sut =>
    val anyFunction: Any => Either[Any, Any] = v => Right(v)
    assert(sut.traverse[Any, Any, Any](List(Right(1), Right(2), Left("Invalid 1"), Right(3), Left("Invalid 2"), Right(4)))(anyFunction)
      == Left("Invalid 1"))
  }

  test("traverse of a List of Right with a function that always return Right should be a List of Right of the applied function") { sut =>
    val half: Int => Right[Double] = v => Right(v / 2d)
    assert(sut.traverse[Any, Int, Double](List(Right(1), Right(2), Right(3), Right(4)))(half)
      == Right(List(0.5d, 1d, 1.5d, 2d)))
  }

  test("traverse of a List of Right with a function that returns at least one Left should return the first Left returned") { sut =>
    val oddOrLeft: Int => Either[String, Int] = v => if (v % 2 != 0) Right(v) else Left(v + " is invalid!")
    assert(sut.traverse[String, Int, Int](List(Right(1), Right(2), Right(3), Right(4)))(oddOrLeft)
      == Left("2 is invalid!"))
  }

}