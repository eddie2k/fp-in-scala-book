package ch3

class ex17 {

  def doubleToString(l: List[Double]): List[String] = {
    l.foldLeft(Nil: List[String])((acc, d) => acc.:+(d.toString()))
  }

}