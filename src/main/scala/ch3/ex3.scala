package ch3

class ex3 {

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 0) throw new IllegalArgumentException("Can't drop negative amount of items")

    l match {
      case Nil                 => Nil
      case x :: xs if (n > 0)  => drop(xs, n - 1)
      case x :: xs if (n == 0) => l
    }
  }
}