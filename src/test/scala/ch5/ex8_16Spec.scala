package ch5

import org.scalatest.fixture.FunSuite

class ex8Spec extends FunSuite {

  type FixtureParam = ex8

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex8()

    try test(fixture)
    finally {}
  }

  //can't really test infinite streams. Probably wrong testing strategy

  test("ones should start with 1s") { sut =>
    assert(((sut ones) take 5 toList) == List(1, 1, 1, 1, 1))
  }

  test("constant Stream should start with the same element") { sut =>
    val anyValue: Any = 123
    assert((sut constant anyValue take 5 toList) == List(anyValue, anyValue, anyValue, anyValue, anyValue))
  }

  test("from of 0 should start with the Natural numbers") { sut =>
    assert((sut from 0 take 5 toList) == List(0, 1, 2, 3, 4))
  }

  test("fibs should start with the first fibonaccy numbers") { sut =>
    assert((sut.fibs take 7 toList) == List(0, 1, 1, 2, 3, 5, 8))
  }

  test("map of the empty Stream should return the empty Stream") { sut =>
    val anyFunction: Any => Boolean = _ => false
    assert(sut.map(Stream.empty)(anyFunction).toList() == Stream.empty.toList())
  }

  test("map of a non-empty Stream should apply the function to each element") { sut =>
    val s = Stream(1, 2, 3, 4, 5)
    val twice: Int => Int = _ * 2

    assert((sut.map(s)(twice) toList) == Stream(2, 4, 6, 8, 10).toList())
  }

  test("take 0 elements from the empty Stream should return the empty Stream") { sut =>
    assert(sut.take(Stream.empty, 0).toList == Stream.empty.toList())
  }

  test("take 0 elements from a non-empty Stream should return the empty Stream") { sut =>
    val s = Stream(1, 2)
    assert(sut.take(s, 0).toList == Stream.empty.toList())
  }

  test("take N elements (fewer elements than present in the Stream) should return N first elements") { sut =>
    val s = Stream(1, 2, 3)
    assert(sut.take(s, 2).toList == List(1, 2))
  }

  test("take as many elements as present in the Stream should return all of them") { sut =>
    val s = Stream(1, 2, 3)
    assert(sut.take(s, 3).toList == List(1, 2, 3))
  }

  test("take more elements than present in the Stream should return all of them") { sut =>
    val s = Stream(1, 2, 3)
    assert(sut.take(s, 5).toList == List(1, 2, 3))
  }

  test("takeWhile for the empty Stream should return the empty Stream") { sut =>
    val s = Stream.empty
    val anyFunction: Any => Boolean = _ => false

    assert(sut.takeWhile(s)(anyFunction).toList() == Stream.empty.toList())
  }

  test("takeWhile for a non-empty Stream with a function that returns always true should return the entire Stream") { sut =>
    val s = Stream(1, 2, 3, 4, 5)
    val alwaysTrue: Any => Boolean = _ => true

    assert(sut.takeWhile(s)(alwaysTrue).toList() == List(1, 2, 3, 4, 5))
  }

  test("takeWhile should return the first sequence that meets the predicate") { sut =>
    val s = Stream(1, 1, 1, 2, 2)
    val isOdd: Int => Boolean = _ % 2 == 1

    assert(sut.takeWhile(s)(isOdd).toList() == List(1, 1, 1))
  }

  test("zipWith of two empty streams should return the empty stream") { sut =>
    def any(x: Any, y: Any): Any = AnyRef
    assert(sut.zipWith(Stream.empty, Stream.empty)(any(_, _)) == Stream.empty)
  }

  test("zipWith of non-empty and empty streams should return the empty stream") { sut =>
    def any(x: Any, y: Any): Any = AnyRef
    assert(sut.zipWith(Stream(1, 2, 3), Stream.empty)(any) == Stream.empty)
  }

  test("zipWith of empty and non-empty streams should return the empty stream") { sut =>
    def any(x: Any, y: Any): Any = AnyRef
    assert(sut.zipWith(Stream.empty, Stream(1, 2, 3))(any) == Stream.empty)
  }

  test("zipWith two non-empty streams of the same size should apply the function to all the pairs of both streams") { sut =>
    def sumIntAndLenght(x: Int, s: String): Int = x + s.length()
    assert(sut.zipWith(Stream(1, 2, 3), Stream("1", "22", "333"))(sumIntAndLenght).toList() == List(2, 4, 6))
  }

  test("zipWith two non-empty streams of different size should apply the function only to pairs present in both streams") { sut =>
    def sumIntAndLenght(x: Int, s: String): Int = x + s.length()
    assert(sut.zipWith(Stream(1, 2, 3, 4, 5), Stream("1", "22", "333"))(sumIntAndLenght).toList() == List(2, 4, 6))
  }

  test("zipAll of two empty streams should return the empty stream") { sut =>
    assert(sut.zipAll(Stream.empty, Stream.empty) == Stream.empty)
  }

  test("zipAll of non-empty and empty streams should return all the elements from the non-empty paired with None's") { sut =>
    assert(sut.zipAll(Stream(1, 2, 3), Stream.empty).toList() == List((Some(1), None), (Some(2), None), (Some(3), None)))
  }

  test("zipAll of empty and non-empty streams should return all the elements from the non-empty paired with None's") { sut =>
    assert(sut.zipAll(Stream.empty, Stream(1, 2, 3)).toList() == List((None, Some(1)), (None, Some(2)), (None, Some(3))))
  }

  test("zipAll of two non-empty streams, being the first the longest should return all the possible pairs and combine missing elements with None") { sut =>
    assert(sut.zipAll(Stream(1, 2, 3), Stream("a", "b")).toList() == List((Some(1), Some("a")), (Some(2), Some("b")), (Some(3), None)))
  }

  test("zipAll of two non-empty streams, being the second the longest should return all the possible pairs and combine missing elements with None") { sut =>
    assert(sut.zipAll(Stream(1, 2), Stream("a", "b", "c")).toList() == List((Some(1), Some("a")), (Some(2), Some("b")), (None, Some("c"))))
  }

  test("zipAll of two non-empty streams of the same size should return all the possible pairs") { sut =>
    assert(sut.zipAll(Stream(1, 2, 3), Stream("a", "b", "c")).toList() == List((Some(1), Some("a")), (Some(2), Some("b")), (Some(3), Some("c"))))
  }

  test("Empty stream startsWith empty Stream should be true") { sut =>
    assert((sut.startsWith(Stream.empty, Stream.empty)))
  }

  test("Non-empty stream startsWith empty Stream should be true") { sut =>
    assert((sut.startsWith(Stream(1, 2, 3), Stream.empty)))
  }

  test("finite stream 1,2,3 startsWith 1,2 should be true") { sut =>
    val s = Stream(1, 2, 3)
    val sub = Stream(1, 2)

    assert(sut.startsWith(s, sub))
  }

  test("finite stream 1,2 startsWith 1,2,3 should be false") { sut =>
    val s = Stream(1, 2)
    val sub = Stream(1, 2, 3)

    assert(!sut.startsWith(s, sub))
  }

  test("infinite stream 1,2,3... startsWith 1,2,3 should be true") { sut =>
    val s = new ex8().from(1)
    val sub = Stream(1, 2, 3)

    assert(sut.startsWith(s, sub))
  }

  test("infinite stream 1,2,3 startsWith 2,3 should be false") { sut =>
    val s = new ex8().from(1)
    val sub = Stream(2, 3)

    assert(!sut.startsWith(s, sub))
  }

  test("tails of the empty stream should be the empty stream") { sut =>
    assert(sut.tails(Stream.empty).map(_.toList).toList() == List(Nil))
  }

  test("tails of Stream(1,2,3) should be the Stream(Stream(1,2,3), Stream(2,3), Stream(3) ,Stream()) stream") { sut =>
    assert((sut tails Stream(1, 2, 3)).map(_.toList).toList() == List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

  test("scanRight of empty Stream with op=_ + _ and z=0 should be 0 ") { sut =>
    assert(sut.scanRight[Int, Int](Stream.empty)(0)(_ + _).toList == List(0))
  }

  test("scanRight of 1,2,3 with op=_ + _ and z=0 should be 6,5,3,0 ") { sut =>
    assert(sut.scanRight(Stream(1, 2, 3))(0)(_ + _).toList == List(6, 5, 3, 0))
  }

}

