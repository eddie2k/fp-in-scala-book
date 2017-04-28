package ch4

import org.scalatest.fixture.FunSuite

class ex2Spec extends FunSuite {

  type FixtureParam = ex2

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex2()

    try test(fixture)
    finally {

    }
  }

  test("variance of empty sequence is non-existent") { sut =>
    assert(sut.variance(Seq.empty) == None)
  }

  test("variance of non-empty seq of equal elements is 0 ") { sut =>
    val any = 3
    assert(sut.variance(List(any, any, any)) == Some(0))
  }

  test("variance of 0, 3, 6 is 6") { sut =>
    val any = 3
    assert(sut.variance(List(0, 3, 6)) == Some(6))
  }

}