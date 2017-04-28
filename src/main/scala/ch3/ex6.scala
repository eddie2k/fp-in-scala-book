package ch3

class ex6 {

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil      => throw new IllegalArgumentException("")
      case x :: Nil => Nil
      case x :: xs  => x :: init(xs)
    }
  }
}