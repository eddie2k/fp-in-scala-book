package ch4

class ex5 {

  def sequence[A](l: List[Option[A]]): Option[List[A]] = {

    def concat[A](olist: Option[List[A]], o: Option[A]): Option[List[A]] = {
      o.flatMap(v => olist.map(_.:+(v)))
    }

    def concat_2[A](olist: Option[List[A]], o: Option[A]): Option[List[A]] = {
      //Alternative version using function from previous exercise
      val ex3_4 = new ex3_4
      ex3_4.map2(olist, o)((x, y) => x.:+(y))
    }

    l.foldLeft(Some(Nil): Option[List[A]])(concat_2)
  }
}