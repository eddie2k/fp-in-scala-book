package ch4

import org.scalatest.fixture.FunSuite

class ex6Spec extends FunSuite {

  type FixtureParam = ex6

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex6()

    try test(fixture)
    finally {

    }
  }

  test("traverse of an empty list should return Some(Empty)") { sut =>
    val anyFunction = (a: Any) => None
    assert(sut.traverse(Nil: List[Any])(anyFunction) == Some(Nil))
  }

  test("traverse of a non-empty list with a function that always return Some should process all the elements") { sut =>
    val alwaysSome = (a: Int) => Some(a)
    assert(sut.traverse(List(1, 2, 3))(alwaysSome) == Some(List(1, 2, 3)))
  }

  test("traverse of a non-empty list with a function that return None at least once should return None") { sut =>
    val evenOrNone = (a: Int) => if (a % 2 == 0) Some(a) else None
    assert(sut.traverse(List(1, 2, 3))(evenOrNone) == None)
  }

  test("sequence of Empty list should be Some(Nil)") { sut =>
    assert(sut.sequence(Nil) == Some(Nil))
  }

  test("sequence of List containing None should be None") { sut =>
    assert(sut.sequence(List(None)) == None)
  }

  test("sequence of List containing only Some should be Some of the all elements ") { sut =>
    assert(sut.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  }

  test("sequence of List containing mix of Some and None should be None") { sut =>
    assert(sut.sequence(List(Some(1), Some(2), None, Some(3))) == None)
  }

}