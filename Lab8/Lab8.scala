object Lab8 extends App {

  //Zadanie 1
  def sum(seq: Seq[Option[Double]]): Double = {
    seq.map((d: Option[Double]) => d match {
      case Some(i) => i
      case None => 0.0
    }).sum
  }

  //Zadanie 2
  def position[A](seq: Seq[A], el: A): Option[Int] = {
    val check = (r: Option[Int], e: (A, Int)) => e match {
      case (x, i) if x == el && r.isEmpty => Some(i)
      case _ => r
    }
    seq.zipWithIndex.foldLeft(None: Option[Int])(check)
  }

  //Zadanie 3
  def indices[A](seq: Seq[A], el: A): Set[Int] = {

    val addIndices = (r: Seq[(A, Int)], a: A) => r match {
      case Seq() => (a, 0) +: r
      case _ => (a, r.head._2 + 1) +: r
    }

    val check = (s : Set[Int], e: (A, Int)) => e match {
      case (x, i) if x == el => s + i
      case _ => s
    }

    seq.foldLeft(Seq[(A, Int)]())(addIndices).reverse.foldLeft(Set[Int]())(check)
  }

  //Zadanie 4
  def swap[A](seq: Seq[A]): Seq[A] = {

    val addIndices = (r: Seq[(A, Int)], a: A) => r match {
      case Seq() => (a, 0) +: r
      case _ => (a, r.head._2 + 1) +: r
    }

    val folding = (r: Seq[(A, Int)], p: (A, Int)) => p._2 % 2 match {
      case 0 => p +: r
      case _ => r.head +: p +: r.tail
    }

    seq.foldLeft(Seq[(A, Int)]())(addIndices).reverse.foldLeft(Seq[(A, Int)]())(folding).reverse.map((p: (A, Int)) => p._1)
  }

  //Zadanie 5
  def europeanZonesSorted(): Seq[String] = {
    val zones: Seq[String] = java.util.TimeZone.getAvailableIDs.toSeq
    println(zones)
    zones.filter( (s: String) => s.startsWith("Europe") ).map((s: String) => s.stripPrefix("Europe/"))
      .sortWith((a: String, b: String) => if(a.length == b.length) { a < b } else a.length < b.length)
      .map((s: String) => "Europe/" + s)
  }

}
