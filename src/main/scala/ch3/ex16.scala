package ch3

class ex16 {

  def add1(l: List[Int]): List[Int] = {
    l.foldLeft(Nil: List[Int])((acc, e) => acc.:+(e + 1))
  }
}