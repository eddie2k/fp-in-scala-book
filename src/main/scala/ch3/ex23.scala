package ch3

class ex23 {

  def zipWith[A, B, C](x: List[A], y: List[B])(f: (A, B) => C): List[C] = {
    (x, y) match {
      case (_, Nil)           => Nil
      case (Nil, _)           => Nil
      case (a :: as, b :: bs) => (f(a, b)) :: zipWith(as, bs)(f)
    }
  }
}