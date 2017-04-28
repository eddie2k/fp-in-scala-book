package ch3

class ex12 {

  def reverse[A](l: List[A]): List[A] = {
    l.foldLeft(List[A]())((acc, x) => x :: acc)
  }

}