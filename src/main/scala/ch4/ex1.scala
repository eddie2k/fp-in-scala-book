package ch4

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case None    => None
      case Some(a) => Some(f(a))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse (None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None    => default
      case Some(a) => a
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(a => a) orElse (ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    def g(a: A): Option[A] = if (f(a)) this else None
    flatMap(g)
  }
}