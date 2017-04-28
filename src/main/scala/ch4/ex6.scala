package ch4

class ex6 {

  def traverse[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] = {

    def concat[A](olist: Option[List[A]], o: Option[A]): Option[List[A]] = {
      o.flatMap(v => olist.map(_.:+(v)))
    }

    l.foldLeft(Some(Nil): Option[List[B]])((acc: Option[List[B]], a: A) => concat(acc, f(a)))
  }

  def sequence[A](l: List[Option[A]]): Option[List[A]] = {
    traverse[Option[A], A](l)(Predef.identity(_))
  }

}