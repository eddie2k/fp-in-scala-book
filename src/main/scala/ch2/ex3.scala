

package ch2

class ex3 {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b: B => f(a, b)
  }
}