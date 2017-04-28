package ch3

class ex18 {

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    l.foldLeft(Nil: List[B])((acc, e) => acc.:+(f(e)))
  }
}