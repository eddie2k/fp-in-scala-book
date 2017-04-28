package ch5

class ex8 {

  def ones(): Stream[Int] = {
    //By hand
    //    Stream.cons(1, ones())

    //Using general unfold
    val z: Int = 1
    val op: Int => Option[(Int, Int)] = x => Some(x, x)

    unfold(z)(op)
  }

  def constant[A](a: A): Stream[A] = {
    //By hand
    //    Stream.cons[A](a, constant(a))

    //Using general unfold
    val z: A = a
    val op: A => Option[(A, A)] = x => Some(x, x)
    unfold(z)(op)
  }

  def from(n: Int): Stream[Int] = {
    //By hand
    Stream.cons[Int](n, from(n + 1))

    //Using general unfold
    val z: Int = n
    val op: Int => Option[(Int, Int)] = x => Some(x, x + 1)

    unfold(z)(op)
  }

  def fibs(): Stream[Int] = {
    // My first, own-version
    //    def generateFrom(x: Int, y: Int): Stream[Int] = {
    //      Stream.cons(x + y, generateFrom(y, x + y))
    //    }
    //    Stream.cons(0, Stream.cons(1, generateFrom(0, 1)))

    //As proposed in solutions
    //    def go(a: Int, b: Int): Stream[Int] = {
    //      Stream.cons(a, go(b, a + b))
    //    }
    //    go(0, 1)

    //Using general unfold
    val z: (Int, Int) = (0, 1)
    val op: ((Int, Int)) => Option[(Int, (Int, Int))] = (pair) => Some((pair._1, (pair._2, pair._1 + pair._2)))

    unfold[Int, (Int, Int)](z)(op)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None         => Stream.empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  // USING UNFOLD: map, take, takeWhile, zipWith, and zipAll
  def map[A, B](s: Stream[A])(f: A => B): Stream[B] = {
    val z: Stream[A] = s
    val op: Stream[A] => Option[(B, Stream[A])] = _ match {
      case Empty      => None
      case Cons(h, t) => Some(f(h()), t())
    }

    unfold[B, Stream[A]](z)(op)
  }

  def take[A](s: Stream[A], n: Int): Stream[A] = {
    val z: (Stream[A], Int) = (s, n)
    val op: ((Stream[A], Int)) => Option[(A, (Stream[A], Int))] = (pair) => pair._1 match {
      case Cons(h, t) if (pair._2 > 0) => Some(h(), (t(), pair._2 - 1))
      case _                           => None
    }

    unfold[A, (Stream[A], Int)](z)(op)
  }

  def takeWhile[A](s: Stream[A])(f: A => Boolean): Stream[A] = {
    val z: Stream[A] = s
    val op: (Stream[A]) => Option[(A, Stream[A])] =
      _ match {
        case Cons(h, t) if (f(h())) => Some(h(), t())
        case _                      => None
      }

    unfold(z)(op)
  }

  def zipWith[A, B, C](s1: Stream[A], s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    val z = (s1, s2)
    val op: ((Stream[A], Stream[B])) => Option[(C, (Stream[A], Stream[B]))] =
      _ match {
        case ((Cons(h1, t1)), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _                              => None
      }

    unfold(z)(op)
  }

  def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    val z: (Stream[A], Stream[B]) = (s1, s2)
    val op: ((Stream[A], Stream[B])) => Option[((Option[A], Option[B]), (Stream[A], Stream[B]))] =
      _ match {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (Cons(h1, t1), Empty)        => Some((Some(h1()), None), (t1(), Empty))
        case (Empty, Cons(h2, t2))        => Some((None, Some(h2())), (Empty, t2()))
        case (Empty, Empty)               => None
      }

    unfold(z)(op)
  }

  def startsWith[A](s: Stream[A], sub: Stream[A]): Boolean = {
    // First version
    //    zipAll(s, sub).map[Option[Boolean]]({
    //      case (Some(a), Some(b)) => Some(a == b)
    //      case (Some(_), None)    => None
    //      case (None, Some(_))    => Some(false)
    //      case (None, None)       => Some(true)
    //    })
    //      .takeWhile(_ match {
    //        case None => false
    //        case _    => true
    //      })
    //      .foldLeft[Option[Boolean]](Some(true))((acc, x) => acc.flatMap(accs => x.map(x => accs && x)))
    //      .getOrElse(true)

    // Simplified version:
    //    1. Removed unnecessary map
    //    2. Simplified predicate in takeWhile by using Option.isEmpty
    //    3. Replaced fold-getOrElse by a forAll
    zipAll[A, A](s, sub)
      .takeWhile(!_._2.isEmpty)
      .forAll(p => p._1 == p._2)
  }

  def tails[A](s: Stream[A]): Stream[Stream[A]] = {
    val z: Stream[A] = s
    val op: Stream[A] => Option[(Stream[A], Stream[A])] = _ match {
      case Cons(h, t) => Some(Stream.cons(h(), t()), t())
      case Empty      => None
    }

    unfold(z)(op).append(Stream(Stream.empty))
  }

  def scanRight[A, B](s: Stream[A])(z: B)(f: (A, => B) => B): Stream[B] = {
    val zz: Stream[B] = Stream(z)
    val op: (A, => Stream[B]) => Stream[B] = (x, acc) => acc match {
      case Cons(h, t) =>
        val hh = f(x, h())
        Stream.cons(hh, acc)
      case Empty => acc
    }

    s.foldRight(zz)(op)
  }
}

