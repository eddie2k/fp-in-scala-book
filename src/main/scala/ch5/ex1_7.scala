package ch5

import scala.annotation.tailrec

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def toList(): List[A] = {
    //    Non tail recursive
    //    this match {
    //      case Empty      => Nil
    //      case Cons(h, t) => h() :: t().toList()
    //    }

    //Tail recursive
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Empty      => acc
        case Cons(h, t) => go(t(), acc :+ h())
      }
    }

    go(this, Nil)
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Empty                  => Stream.empty
      case Cons(h, t) if (n == 0) => Stream.empty
      case Cons(h, t)             => Stream.cons(h(), t().take(n - 1))
    }
  }

  def takeWhile(f: A => Boolean): Stream[A] = {
    //    //Hand-made
    //    this match {
    //      case Empty                   => Stream.empty
    //      case Cons(h, _) if (!f(h())) => Stream.empty
    //      case Cons(h, t)              => Stream.cons(h(), t().takeWhile(f))
    //    }

    //Fold-right version
    val z = Stream.empty
    val op: (A, => ch5.Stream[A]) => ch5.Stream[A] = (x, acc) => if (f(x)) Stream.cons(x, acc) else Stream.empty
    foldRight[Stream[A]](z)(op)
  }

  def foldRight[B](z: => B)(op: (A, => B) => B): B = {
    this match {
      case Empty      => z
      case Cons(h, t) => op(h(), t().foldRight(z)(op))
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    //Hand-made
    //    this match {
    //      case Empty      => true
    //      case Cons(h, t) => p(h()) && t().forAll(p)
    //    }

    //Fold-right version
    foldRight(true)((x, acc) => p(x) && acc)
  }

  def map[B](f: A => B): Stream[B] = {
    val z: Stream[B] = Stream.empty
    val op: (A, => Stream[B]) => Stream[B] = (x, acc) => Stream.cons(f(x), acc)

    foldRight[Stream[B]](z)(op)
  }

  def filter(f: A => Boolean): Stream[A] = {
    val z: Stream[A] = Stream.empty
    val op: (A, => Stream[A]) => Stream[A] = (x, acc) => if (f(x)) Stream.cons(x, acc) else acc

    foldRight(z)(op)
  }

  def append[B >: A](other: Stream[B]): Stream[B] = {
    val z: Stream[B] = other
    val op: (B, => Stream[B]) => Stream[B] = (x, acc) => Stream.cons(x, acc)

    foldRight(z)(op)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    val z: Stream[B] = Stream.empty
    val op: (A, => Stream[B]) => Stream[B] = (x, acc) => f(x) append acc

    foldRight(z)(op)
  }
}

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

