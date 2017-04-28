package ch5

import org.scalatest.FunSuite

class ex1_7Spec extends FunSuite {

  override def withFixture(test: NoArgTest) = {

    try test()
    finally {}
  }

  test("toList of the empty stream should be the empty list") {
    assert(Stream.empty.toList() == Nil)
  }

  test("toList of a single-element stream should return a List with that element only")({
    val anyValue = 1
    val s = Stream(anyValue)

    assert(s.toList() == List(anyValue))
  })

  test("toList of a multi-element stream should return a List with all the elements in the same order") {
    val s = Stream(1, 2, 3)

    assert(s.toList() == List(1, 2, 3))
  }

  test("take 0 elements from the empty Stream should return the empty Stream") {
    assert(Stream.empty.take(0).toList == Stream.empty.toList())
  }

  test("take 0 elements from a non-empty Stream should return the empty Stream") {
    val s = Stream(1, 2)
    assert(s.take(0).toList == Stream.empty.toList())
  }

  test("take N elements (fewer elements than present in the Stream) should return N first elements") {
    val s = Stream(1, 2, 3)
    assert(s.take(2).toList == List(1, 2))
  }

  test("take as many elements as present in the Stream should return all of them") {
    val s = Stream(1, 2, 3)
    assert(s.take(3).toList == List(1, 2, 3))
  }

  test("take more elements than present in the Stream should return all of them") {
    val s = Stream(1, 2, 3)
    assert(s.take(5).toList == List(1, 2, 3))
  }

  test("takeWhile for the empty Stream should return the empty Stream") {
    val s = Stream.empty
    val anyFunction: Any => Boolean = _ => false

    assert(s.takeWhile(anyFunction).toList() == Stream.empty.toList())
  }

  test("takeWhile for a non-empty Stream with a function that returns always true should return the entire Stream") {
    val s = Stream(1, 2, 3, 4, 5)
    val alwaysTrue: Any => Boolean = _ => true

    assert(s.takeWhile(alwaysTrue).toList() == List(1, 2, 3, 4, 5))
  }

  test("takeWhile should return the first sequence that meets the predicate") {
    val s = Stream(1, 1, 1, 2, 2)
    val isOdd: Int => Boolean = _ % 2 == 1

    assert(s.takeWhile(isOdd).toList() == List(1, 1, 1))
  }

  test("forAll for the empty Stream should be true") {
    val s = Stream.empty
    val anyFunction: Any => Boolean = _ => false

    assert(s.forAll(anyFunction))
  }

  test("forAll with a function that returns always true should return true") {
    val s = Stream(1, 2, 3, 4, 5)
    val alwaysTrue: Any => Boolean = _ => true

    assert(s.forAll(alwaysTrue))
  }

  test("forAll with a function that returns false at least once should return false") {
    val s = Stream(1, 2, 3, 4, 5, 4, 3, 2, 1)
    val lessThanFive: Int => Boolean = _ < 5

    assert(!s.forAll(lessThanFive))
  }

  test("map of the empty Stream should return the empty Stream") {
    val anyFunction: Any => Boolean = _ => false
    assert(Stream.empty.map(anyFunction).toList() == Stream.empty.toList())
  }

  test("map of a non-empty Stream should apply the function to each element") {
    val s = Stream(1, 2, 3, 4, 5)
    val twice: Int => Int = _ * 2

    assert((s map twice toList) == Stream(2, 4, 6, 8, 10).toList())
  }

  test("filter the empty Stream should return the empty Stream") {
    val anyFunction: Any => Boolean = _ => false
    assert(Stream.empty.filter(anyFunction).toList() == Stream.empty.toList())
  }

  test("filter of non-empty Stream with a function that always return false should return the empty Stream") {
    val s = Stream(1, 2, 3, 4, 5)
    val alwaysFalse: Int => Boolean = _ => false

    assert((s filter alwaysFalse toList) == Stream.empty.toList())
  }

  test("filter of non-empty Stream should keep only elements that meet the predicate") {
    val s = Stream(1, 2, 3, 4, 5)
    val isOdd: Int => Boolean = _ % 2 == 1

    assert((s filter isOdd toList) == List(1, 3, 5))
  }

  test("append two empty Streams should return the empty Stream") {
    val s1: Stream[Int] = Stream.empty
    val s2: Stream[Int] = Stream.empty

    assert((s1 append s2 toList) == Stream.empty.toList())
  }

  test("append a non-empty to an empty Stream should return the non-empty Stream") {
    val s1: Stream[Int] = Stream.empty
    val s2: Stream[Int] = Stream(1, 2, 3)

    assert((s1 append s2 toList) == List(1, 2, 3))
  }

  test("append an empty Steam to a non-empty Stream should return the non-empty stream") {
    val s1: Stream[Int] = Stream.empty
    val s2: Stream[Int] = Stream(1, 2, 3)

    assert((s1 append s2 toList) == List(1, 2, 3))
  }

  test("append two non-empty Steam should return a Stream with the previous elements and the new elements") {
    val s1: Stream[Int] = Stream(1, 2, 3)
    val s2: Stream[Int] = Stream(4, 5, 6)

    assert((s1 append s2 toList) == List(1, 2, 3, 4, 5, 6))
  }

  test("flatMap in an empty Stream should return the empty Stream") {
    val s = Stream.empty
    val anyFunction: Any => Stream[Any] = _ => Stream(1, 2, 3)

    assert((s flatMap anyFunction toList) == Stream.empty.toList())
  }

  test("flatMap of a non-empty Stream with a function that always return the empty Stream should return the empty Stream") {
    val s = Stream(1, 2, 3)
    val alwaysEmptyStream: Any => Stream[Any] = _ => Stream.empty

    assert((s flatMap alwaysEmptyStream toList) == Stream.empty.toList())
  }

  test("flatMap of a non-empty Stream should apply the given function to every element and append the results") {
    val s = Stream(1, 2, 3)
    val repeatElement: Int => Stream[Int] = i => Stream(i, i)

    assert((s flatMap repeatElement toList) == Stream(1, 1, 2, 2, 3, 3).toList())
  }

}

