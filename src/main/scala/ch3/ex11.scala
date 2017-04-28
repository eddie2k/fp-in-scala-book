package ch3

class ex11 {
  //  sum , product , and a function to compute the length

  def sum(l: List[Int]): Int = {
    l.foldLeft(0)(_ + _)
  }

  def product(l: List[Int]): Int = {
    l.foldLeft(1)(_ * _)
  }

  def length[A](l: List[A]): Int = {
    l.foldLeft(0)((acc, _) => acc + 1)
  }
}