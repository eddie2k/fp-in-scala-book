package ch3

class ex21 {

  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    l.flatMap(x => if (f(x)) List(x) else Nil)
  }
}