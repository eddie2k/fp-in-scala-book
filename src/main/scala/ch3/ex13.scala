package ch3

class ex13 {

  def foldLeft[A, B](l: List[A], z: B)(op: (B, A) => B): B = {

    def swappedArgsOp(x: A, acc: B): B = op(acc, x)

    l.reverse.foldRight(z)(swappedArgsOp)
  }

  def foldRight[A, B](l: List[A], z: B)(op: (A, B) => B): B = {

    def swappedArgsOp(acc: B, x: A): B = op(x, acc)

    l.reverse.foldLeft(z)(swappedArgsOp)
  }
}