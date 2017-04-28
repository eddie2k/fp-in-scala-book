package ch3

class ex2 {

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil     => throw new IllegalArgumentException("tail of empty list")
      case x :: xs => xs
    }
  }

}