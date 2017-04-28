package ch3

import org.scalatest.fixture.FunSuite

class ex14Spec extends FunSuite {

  type FixtureParam = ex14

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex14()

    try test(fixture)
    finally {

    }
  }

  test("append to the empty list should return single-element list") { sut =>
    val any = 2
    assert(sut.append(any, Nil) == List(any))
  }

  test("append to a non-empty list should return same list with the element at the beginning") { sut =>
    assert(sut.append(0, List(1, 2, 3, 4, 5)) == List(0, 1, 2, 3, 4, 5))
  }

  
  test("append two empty lists should return the list") { sut =>
    assert(sut.append(Nil, Nil) == Nil)
  }

  test("append empty and non-empty lists should return the non-empty list") { sut =>
    assert(sut.append(Nil, List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4, 5))
  }

  test("append non-empty and empty lists should return the non-empty list") { sut =>
    assert(sut.append(List(1, 2, 3, 4, 5), Nil) == List(1, 2, 3, 4, 5))
  }

  test("append two non-empty lists should return the concatenation of both") { sut =>
    assert(sut.append(List(1, 2, 3), List(4, 5)) == List(1, 2, 3, 4, 5))
  }

}