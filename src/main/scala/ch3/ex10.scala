package ch3

import scala.annotation.tailrec

class ex10 {

  @tailrec
  final def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil    => z
      case h :: t => foldLeft(t, f(z, h))(f)
    }
  }
}