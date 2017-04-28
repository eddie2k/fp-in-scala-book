package ch3

import scala.annotation.tailrec

class ex24 {
  
  @tailrec
  final def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    (l, sub) match {
      case (_, Nil)           => true
      case (Nil, y :: ys)     => false
      case (x :: xs, y :: ys) => if (x == y) hasSubsequence(xs, ys) else hasSubsequence(xs, sub)
    }
  }
}