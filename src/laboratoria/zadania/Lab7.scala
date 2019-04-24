package laboratoria.zadania

object Lab7 extends App {

  //Zadanie 1
  def subseq[A](seq: Seq[A], begIdx: Int, endIdx: Int): Seq[A] = seq.slice(begIdx, begIdx + endIdx - 1)

  //Zadanie 2
  def pairPosNeg(seq: Seq[Double]): (Seq[Double], Seq[Double]) = seq.filter((n: Double) => n != 0).partition((n: Double) => n < 0)

  //Zadanie 3
  def deStutter[A](seq: Seq[A]): Seq[A] = {
    val f = (seq: Seq[A], e: A) => seq match {
      case Seq() => e+:seq
      case _ if seq.head != e => e+:seq
      case _ => seq
    }
    seq.foldLeft(Seq[A]())(f).reverse
  }

  //Zadanie 4
  def remElems[A](seq: Seq[A], k: Int): Seq[A] = {
    seq.zipWithIndex.filter((pair: (A, Int)) => pair._1 != k).map((pair: (A, Int)) => pair._1)
  }

  //Zadanie 5
  def freq[A](seq: Seq[A]): Map[A, Int] = seq.groupBy((a: A) => a).mapValues((s: Seq[A]) => s.size)
  // (bez mapValues) funkcja GroupBy zwraca typ Map[B, Seq[A]], a nie [A, Int]

  //Zadanie 6
  def countChars(str: String): Int = str.groupBy((c: Char) => c).size

}
