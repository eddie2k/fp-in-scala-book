package ch4

import org.scalatest.fixture.FunSuite

class RightSpec extends FunSuite {

  type FixtureParam = Right[String]
  val anyValue = "anyValue"

  override def withFixture(test: OneArgTest) = {

    val fixture = new Right(anyValue)

    try test(fixture)
    finally {}
  }

  test("map should return a Right object after applying the given function to the inner value") { sut =>
    assert(sut.map[Int](_.length()) == Right(anyValue.length))
  }

  test("flatMap with a Function that returns Right should return a Right object after applying the function") { sut =>
    assert(sut.flatMap[Nothing, Int](v => Right(v.length())) == Right(anyValue.length))
  }

  test("flatMap with a Function that returns Left should return a Left object after applyting the funtion") { sut =>
    assert(sut.flatMap[String, Nothing](v => Left(v + " is invalid!")) == Left("anyValue is invalid!"))
  }

  test("orElse should return the same object") { sut =>
    val another: Either[Nothing, String] = Right("anyString")
    assert(sut.orElse[Nothing, String](another) == sut)
  }

  test("map2 with a Right object should apply the function and wrap the result in a Right") { sut =>
    val b = Right("rightValue")
    assert(sut.map2(b)(_ + "_AND_" + _) == Right("anyValue_AND_rightValue"))
  }

  test("map2 with a Left object should should return the Left object") { sut =>
    val b = Left(anyValue)
    val anyFunction: (Any, Int) => String = (x, y) => ""
    assert(sut.map2(b)(anyFunction) == b)
  }
}

class LeftSpec extends FunSuite {

  type FixtureParam = Left[Any]

  override def withFixture(test: OneArgTest) = {

    val fixture = new Left("anyValue")

    try test(fixture)
    finally {}
  }

  test("map with any function object should return the same object untouched") { sut =>
    val anyFunction = (_: Any) => null
    assert(sut.map(anyFunction) == sut)
  }

  test("flatMap with any function object should return the same object untouched") { sut =>
    val anyFunction = (_: Any) => null
    assert(sut.flatMap(anyFunction) == sut)
  }

  test("orElse should return the other object") { sut =>
    val another: Either[Any, String] = Right("anyString")
    assert(sut.orElse[Any, String](another) == another)
  }

  test("map2 should return the same object") { sut =>
    val anyEither = Right(123)
    val anyFunction: (Any, Int) => String = (x, y) => ""
    assert(sut.map2(anyEither)(anyFunction) == sut)
  }

}