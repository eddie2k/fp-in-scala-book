package ch3

class ex22 {

  def addPairwise(x: List[Int], y: List[Int]): List[Int] = {
    (x, y) match {
      case (_, Nil)           => Nil
      case (Nil, _)           => Nil
      case (a :: as, b :: bs) => (a + b) :: addPairwise(as, bs)
    }
  }
}