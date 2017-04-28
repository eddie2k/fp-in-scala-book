package ch3

import org.scalatest.fixture.FunSuite

class ex29Spec extends FunSuite {

  type FixtureParam = ex29

  override def withFixture(test: OneArgTest) = {

    val fixture = new ex29()

    try test(fixture)
    finally {

    }

  }

    test("size of the leaf tree should be 1") { sut =>
    val any = 3
    val tree = Leaf(any)
    assert(sut.size(tree) == 1)
  }

  test("size of 2-node tree should be 3") { sut =>
    val any = 3
    val tree = Branch(Leaf(any), Leaf(any))
    assert(sut.size(tree) == 3)
  }

  test("size of 3-node tree should be 5") { sut =>
    val any = 3
    val tree = Branch(Leaf(any), Branch(Leaf(any), Leaf(any)))
    assert(sut.size(tree) == 5)
  }

  test("size of 4-node tree should be 7") { sut =>
    val any = 7
    val tree = Branch(Branch(Leaf(any), Leaf(any)), Branch(Leaf(any), Leaf(any)))
    assert(sut.size(tree) == 7)
  }

  test("maximum of the leaf tree is the value of the leaf") { sut =>
    val any = 3
    val tree = Leaf(any)
    assert(sut.maximum(tree) == any)
  }

  test("maximum of 2-node tree should be the biggest value of the two leafs") { sut =>
    val tree = Branch(Leaf(111), Leaf(222))
    assert(sut.maximum(tree) == 222)
  }

  test("maximum of multi-level tree should be the biggest value in the entire tree") { sut =>
    val tree =
      Branch(
        Branch(
          Branch(
            Leaf(40), Leaf(30)),
          Leaf(50)),
        Leaf(100))
    assert(sut.maximum(tree) == 100)
  }

  test("depth of the leaf tree should be 0") { sut =>
    val any = 3
    val tree = Leaf(any)
    assert(sut.depth(tree) == 0)
  }

  test("depth of a multi-level tree should be the depth of the deepest branch ") { sut =>
    val tree =
      Branch(
        Branch(
          Branch(
            Leaf(40), Leaf(30)),
          Leaf(50)),
        Leaf(100))
    assert(sut.depth(tree) == 3)
  }

  test("map of the leaf tree should apply the function to the leaf value") { sut =>
    def twice(x: Int): Int = 2 * x
    val tree = Leaf(1)
    val expected = Leaf(2)
    assert(sut.map(tree)(twice) == expected)
  }

  test("map of the a multi-level tree should apply the function to every single leaf") { sut =>
    def twice(x: Int): Int = 2 * x
    val tree =
      Branch(
        Branch(
          Branch(
            Leaf(40), Leaf(30)),
          Leaf(50)),
        Leaf(100))
    val expected = 
      Branch(
        Branch(
          Branch(
            Leaf(80), Leaf(60)),
          Leaf(100)),
        Leaf(200))
    assert(sut.map(tree)(twice) == expected)
  }

  
}