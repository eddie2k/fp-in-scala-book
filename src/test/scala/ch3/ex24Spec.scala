package ch3

import org.scalatest.fixture.FunSuite

class ex24Spec extends FunSuite {

  type FixtureParam = ex24

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex24()

    try test(fixture)
    finally {

    }

  }

  test("The empty list has the empty list as subsequence") { sut =>
    assert(sut.hasSubsequence(Nil, Nil) == true)
  }

  test("Any non-empty list has the empty list as subsequence") { sut =>
    assert(sut.hasSubsequence(List(1, 2, 3), Nil) == true)
  }

  test("Any list contains itself as subsequence") { sut =>
    assert(sut.hasSubsequence(List(1, 2, 3), List(1, 2, 3)) == true)
  }

  test("A subsequence should be detected in prefix position") { sut =>
    assert(sut.hasSubsequence(List(1, 2, 3), List(1, 2)) == true)
  }

  test("A subsequence should be detected in intermediate position") { sut =>
    assert(sut.hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 4, 5)) == true)
  }

  test("A subsequence of length should be detected") { sut =>
    assert(sut.hasSubsequence(List(1, 2, 3, 4, 5, 6), List(5)) == true)
  }
  
}