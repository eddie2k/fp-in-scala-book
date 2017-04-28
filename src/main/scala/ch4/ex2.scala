package ch4

class ex2 {

  def variance(xs: Seq[Double]): Option[Double] = {
    //Long version
    //    val m: Option[Double] = mean(xs)
    //    val ys: Option[Seq[Double]] = m.flatMap(d => Some(xs.map((v: Double) => math.pow(v - d, 2))))
    //    mean(ys.getOrElse(Seq.empty))

    //Short version
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

}