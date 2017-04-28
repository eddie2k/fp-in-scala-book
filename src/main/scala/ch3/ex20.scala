package ch3

class ex20 {

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    l.foldLeft(Nil: List[B])((acc, x) => acc ::: f(x))
  }

}