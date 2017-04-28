package ch3

class ex29 {

  def fold[A, B](t: Tree[A])(f: (A) => B)(combiner: (B, B) => B): B = {
    t match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => combiner(fold(l)(f)(combiner), fold(r)(f)(combiner))
    }
  }

  def size[A](t: Tree[A]): Int = {
    def const1(x: Any): Int = 1
    def sumAndInc(x: Int, y: Int) = x + y + 1

    fold(t)(const1)(sumAndInc)
  }

  def maximum(t: Tree[Int]): Int = {
    def identity(x: Int) = x

    fold(t)(identity)(_ max _)
  }

  def depth[A](t: Tree[A]): Int = {
    def const0(x: Any): Int = 0
    def maxAndInc(x: Int, y: Int): Int = (x max y) + 1

    fold(t)(const0)(maxAndInc)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    def wrapInLeaf(v: B): Tree[B] = Leaf(v)
    def wrapInBranch(l: Tree[B], r: Tree[B]): Tree[B] = Branch(l, r)

    fold(t)(f andThen wrapInLeaf)(wrapInBranch)
  }
}