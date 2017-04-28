package ch2

import scala.annotation.tailrec


class ex2 {

  def isSorted[A](as: Array[A], lt: (A, A) => Boolean): Boolean = {
    if (lt == null) throw new IllegalArgumentException("Comparator function is null!")
    if (as == null) throw new IllegalArgumentException("Array is null!")

    @tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!lt(as(n), as(n + 1))) false
      else go(n + 1)
    }

    go(0)
  }
}