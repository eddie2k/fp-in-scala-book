package ch3

class ex14 {

  def append[A](x: A, l: List[A]): List[A] = {
    l.foldLeft(List(x))((acc, e) => acc :+ e)
  }

  def append[A](x: List[A], y: List[A]): List[A] = {
    x.foldRight(y)((a, acc) => acc.+:(a))
  }
}