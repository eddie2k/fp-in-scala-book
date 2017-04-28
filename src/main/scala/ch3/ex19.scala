package ch3

class ex19 {
  
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    l.foldLeft(Nil: List[A])((acc, x) => if (f(x)) acc.:+(x) else acc)
  }
}