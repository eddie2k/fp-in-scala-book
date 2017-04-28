package ch4

class ex8 {

  def traverse[E, A, B](l: List[Either[E, A]])(f: A => Either[E, B]): Either[E, List[B]] = {

    val z: Either[E, List[B]] = Right(Nil)
    val op: (Either[E, List[B]], Either[E, A]) => Either[E, List[B]] =
      (acc, x) => acc.flatMap(ll => x.flatMap(f).flatMap(xx => Right(ll.:+(xx))))

    l.foldLeft(z)(op)
  }

  def sequence[E, A](l: List[Either[E, A]]): Either[E, List[A]] = {

    //Fold-based version (from scratch)

    //    val z: Either[E, List[A]] = Right(Nil)
    //    def op: (Either[E, List[A]], Either[E, A]) => Either[E, List[A]] =
    //      (acc, x) => acc.flatMap(ll => x.flatMap(xx => Right(ll.:+(xx))))
    //
    //    l.foldLeft(z)(op)

    //Generalized version from traverse
    traverse[E, A, A](l)(x => Right(x))
  }
}