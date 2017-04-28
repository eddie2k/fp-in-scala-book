package ch2

import org.scalatest.fixture.FunSuite

class ex1Spec extends FunSuite {

  type FixtureParam = ex1

  override def withFixture(test: OneArgTest) = { // Define a shared fixture
    val fixture = new ex1()

    // Shared setup (run at beginning of each test)
    try test(fixture)
    finally {
      // Shared cleanup (run at end of each test)
    }
  }

  test("negative number should throw IllegalArgumentException") { sut =>
    intercept[IllegalArgumentException] {
      sut.fib(-1)
    }
  }

  test("Fib(0) should be 0") { sut =>
    assert(sut.fib(0) == 0)
  }

  test("Fib(1) should be 1") { sut =>
    assert(sut.fib(1) == 1)
  }

  test("Fib(2) should be 1") { sut =>
    assert(sut.fib(2) == 1)
  }

  test("Fib(3) should be 2") { sut =>
    assert(sut.fib(3) == 2)
  }

  test("Fib(4) should be 3") { sut =>
    assert(sut.fib(4) == 3)
  }

  test("Fib(5) should be 5") { sut =>
    assert(sut.fib(5) == 5)
  }
}