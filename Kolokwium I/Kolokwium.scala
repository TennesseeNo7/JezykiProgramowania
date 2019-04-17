package zadania

object Kolokwium {

  def main(args: Array[String]): Unit = {
    val a = Seq((1.0, 2.0), (2.0, 2.5), (2.5, 3.5), (3.0, 5.0), (-1.0, 0.0))
    val b: Seq[(Double, Double)] = Seq()
    println(farthestPair(b))
  }

  //Zadanie 1

  def notDifferentElements[A](seq1: Seq[A], seq2: Seq[A]): Seq[A] = {
    @scala.annotation.tailrec
    def helper(seq1: Seq[A], seq2: Seq[A], out: Seq[A] = Seq()): Seq[A] = (seq1, seq2) match {
      case (Seq(), _) | (_, Seq()) => out.reverse
      case (e1 +: s1, e2 +: s2) if e1 == e2 => helper(s1, s2, e1 +: out)
      case (_ +: s1, _ +: s2) => helper(s1, s2, out)
    }
    helper(seq1, seq2)
  }

  //Zadanie 2

  def sliceMap[A, B](seq: Seq[A], from: Int, until: Int)(op: A => B): Seq[B] = {
    @scala.annotation.tailrec
    def helper(seq: Seq[A], out: Seq[B] = Seq(), ind: Int = 0): Seq[B] = seq match {
      case Seq() => out.reverse
      case _ +: _ if ind > until => out.reverse
      case e +: s if ind >= from => helper(s, op(e) +: out, ind+1)
      case _ +: s => helper(s, out, ind+1)
    }
    helper(seq)
  }

  //Zadanie 3

  def countPair[A, B](seq: Seq[(A, B)])(leq: (A, B) => Boolean): Int = {
    @scala.annotation.tailrec
    def helper(seq: Seq[(A, B)], counter: Int = 0): Int = seq match{
      case Seq() => counter
      case (a, b) +: s if leq(a, b) => helper(s, counter+1)
      case _ +: s => helper(s, counter)
    }
    helper(seq)
  }

  //Zadanie 4

  // max|x - y|
  def farthestPair(seq: Seq[(Double, Double)]): (Double, Double) = {
    @scala.annotation.tailrec
    def helper(seq: Seq[(Double, Double)], out: (Double, Double) = (0.0, 0.0)): (Double, Double) = seq match {
      case Seq() => out
      case (x, y) +: s if math.abs(x - y) > math.abs(out._1 - out._2) => helper(s, (x, y))
      case _ +: s => helper(s, out)
    }
    helper(seq)
  }

}
