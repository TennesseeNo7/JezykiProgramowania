package src.exercises.exams

object Prekolokwium {

  def main(args: Array[String]): Unit = {}

  /**
    * Zwraca ilość słów tej samej długości, które są pod tym samym indeksem ciągu
    */
  def repetition(a: Seq[String], b: Seq[String]): Int = {
    def helper(a: Seq[String], b: Seq[String], counter: Int = 0): Int = (a, b) match {
      case (_, Seq()) | (Seq(), _) => counter
      case (x, y) if x.head.length == y.head.length => helper(x.tail, y.tail, counter + 1)
      case (x, y) => helper(x.tail, y.tail, counter)
    }
    helper(a, b)
  }

  /**
    * Stosuje funkcje 'op1' i 'op2' do wszystick elementów ciągu i tworzy z wyników tych funkcji ciąg par
    */
  def initPairMap[A,B,C](seq: Seq[A])(op1: A =>B)(op2: A => C): Seq[(B,C)] = {
    def helper(seq: Seq[A], out: Seq[(B, C)] = Seq()): Seq[(B,C)] = seq match {
      case Seq() => out.reverse
      case e +: r => helper(r, (op1(e), op2(e)) +: out)
    }
    helper(seq)
  }

  /**
    * Sprawdza czy element na indeksie 'ind' spełnia 'leq'
    */
  def checkElement[A](seq: Seq[A], ind: Int)(leq: A => Boolean): Boolean = {
    def helper(seq: Seq[A], i: Int = 0): Boolean = seq match {
      case Seq() => false
      case e +: _ if i == ind => leq(e)
      case _ +: r => helper(r, i + 1)
    }
    helper(seq)
  }

  /**
    * Przyjmuje ciąg elementów i ich wystąpień, a następnie zwraca ciąg złożony z tych elementów z powtórzeniami
    * ('a', 3), ('b', 2) => aaa, bb
    */
  def inverseCompress[A](seq: Seq[(A, Int)]): Seq[A] = {
    def appendMany(e: (A, Int), out: Seq[A] = Seq()): Seq[A] = e match {
      case (_, 0) => out
      case (x, i) => appendMany((x, i-1), x +: out)
    }
    def helper(seq: Seq[(A, Int)], out: Seq[A] = Seq()): Seq[A] = seq match {
      case Seq() => out
      case a +: r => helper(r, out ++ appendMany(a))
    }
    helper(seq)
  }

}
