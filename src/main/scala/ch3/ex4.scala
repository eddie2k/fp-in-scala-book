package ch3

class ex4 {

  def dropWhile[A](l: List[A])(pred: A => Boolean): List[A] = {
    l match {
      case Nil                   => Nil
      case x :: xs if (pred(x))  => dropWhile(xs)(pred)
      case x :: xs if (!pred(x)) => l
    }
  }
}