package ch3

class ex9 {

  def length[A](l: List[A]): Int = {
    l.foldRight(0)((x, acc) => 1 + acc)
  }
}