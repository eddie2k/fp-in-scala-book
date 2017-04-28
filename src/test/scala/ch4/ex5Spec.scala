package ch4

import org.scalatest.fixture.FunSuite

class ex3_4Spec extends FunSuite {

  type FixtureParam = ex5

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex5

    try test(fixture)
    finally {

    }
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