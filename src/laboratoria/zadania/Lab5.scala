package laboratoria.zadania

object Lab5 extends App {

  // Zadanie 1
  def isOrdered[A](seq: Seq[A])(leq: (A, A) => Boolean): Boolean = seq match {
    case Seq() => true
    case Seq(_) => true
    case e1 +: e2 +: s if leq(e1, e2) => isOrdered[A](e2 +: s)(leq)
    case _ => false
  }

  // Zadanie 2
  // tablica @seq musi byc z zalozenia posortowana wedlug porzadku @leq, inaczej nie ma sensu
  def insertInto[A](a: A, seq: Seq[A])(leq: (A, A) => Boolean): Seq[A] = {
    def insertIntoAux(a: A, seq: Seq[A])(leq: (A, A) => Boolean)(head: Seq[A] = Seq()): Seq[A] = seq match {
      case e +: s if leq(a, e) => (head :+ a :+ e) ++ s
      case e +: s => insertIntoAux(a, s)(leq)(head :+ e)
      case Seq(e) if leq(a, e) => head :+ a :+ e
      case Seq(e) => head :+ e :+ a
    }
    insertIntoAux(a, seq)(leq)()
  }

  // Zadanie 3
  def deStutter[A](seq: Seq[A]): Seq[A] = {
    def deStutterAux(seq: Seq[A], des: Seq[A] = Seq()): Seq[A] = seq match {
      case Seq(x, y) if x != y => des :+ x :+ y
      case Seq(_, y) => des :+ y
      case x +: y +: s if x != y => deStutterAux(y +: s, des :+ x)
      case _ +: y +: s => deStutterAux(y +: s, des)
    }
    deStutterAux(seq)
  }

  // Zadanie 4
  def compress[A](seq: Seq[A]): Seq[(A, Int)] = {
    def compressAux(seq: Seq[A], com: Seq[(A, Int)] = Seq(), count: Int = 1): Seq[(A, Int)] = seq match {
      case Seq(x, y) if x != y => com :+ (x, count) :+ (y, 1)
      case Seq(_, y) => com :+ (y, count + 1)
      case x +: y +: s if x != y => compressAux(y +: s, com :+ (x, count))
      case _ +: y +: s => compressAux(y +: s, com, count + 1)
    }
    compressAux(seq)
  }

}
