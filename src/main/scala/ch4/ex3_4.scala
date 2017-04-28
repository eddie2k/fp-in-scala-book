package ch4

import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException

class ex3_4 {

  def pattern(s: String): Option[Pattern] = {
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }
  }

  def mkMatcher(pat: String): Option[String => Boolean] = {
    //Option's map-orElse style  
    pattern(pat).map(p => (s: String) => p.matcher(s).matches).orElse(None)
  }

  def mkMatcher_1(pat: String): Option[String => Boolean] = {
    //For comprehension style
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches())
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      x <- a
      y <- b
    } yield f(x, y)
  }

  def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] = {
    for {
      f <- mkMatcher(pat1)
      g <- mkMatcher(pat2)
    } yield (f(s) && g(s))
  }

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat1), mkMatcher(pat2))((f, g) => f(s) && g(s))
  }

}