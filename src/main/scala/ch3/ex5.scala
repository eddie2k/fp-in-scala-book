package ch3

class ex5 {

  def setHead[A](l: List[A], a: A): List[A] = {
    l match {
      case Nil     => throw new IllegalArgumentException("setHead of empty list")
      case x :: xs => a :: xs
    }
  }

}