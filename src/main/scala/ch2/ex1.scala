package ch2

import scala.annotation.tailrec

class ex1 {

  def fib(n: Int): Int = {

    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if (n <= 0) prev
      else loop(n - 1, cur, prev + cur)
    }

    if (n < 0) throw new IllegalArgumentException("Fib of negative number is not defined!")
    else loop(n, 0, 1)
  }
}