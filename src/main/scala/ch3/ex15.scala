package ch3

class ex15 {

  def concat[A](l: List[List[A]]): List[A] = {

    def append[A](acc:List[A], x:List[A]) = (new ex14()).append(acc, x)

    l.foldLeft(Nil: List[A])((acc, x) => append(acc, x))
  }
}