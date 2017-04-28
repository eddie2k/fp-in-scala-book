package ch3

import org.scalatest._

class ex11Spec extends fixture.FunSuite {
  type FixtureParam = ex11 // Define the type of the passed fixture object

  override def withFixture(test: OneArgTest) = { // Define a shared fixture
    // Shared setup (run at beginning of each test)
    val fixture = new ex11()

    try test(fixture)
    finally {
      // Shared cleanup (run at end of each test)
    }
  }

  test("sum of empty list should return 0") { sut =>
    assert(sut.sum(Nil) == 0)
  }

  test("sum of non-empty list should return the sum of all the elements") { sut =>
    assert(sut.sum(List(1, 2, 3, 4, 5)) == 15)
  }

  test("product of empty list should return 1") { sut =>
    assert(sut.product(Nil) == 1)
  }

  test("product of non-empty list should return the product of all the elements") { sut =>
    assert(sut.product(List(1, 2, 3, 4, 5)) == 120)
  }

  test("length of empty list should be 0") { sut =>
    assert(sut.length(Nil) == 0)
  }

  test("lenght of non-empty list should match number of elements") { sut =>
    assert(sut.length(List(1, 2, 3, 4, 5)) == 5)
  }

}