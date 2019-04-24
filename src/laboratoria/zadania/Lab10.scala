package laboratoria.zadania

import scala.io.Source

object Lab10 extends App {

  //Zadanie 2
  val names: Seq[String] = Source.fromFile("src\\laboratoria\\zadania\\osoby.txt").getLines.toList
  val mappedNames =  names.groupBy((s: String) => s.length - 1)

  //Zadanie 3
  val numbers: Seq[Int] = Source.fromFile("src\\laboratoria\\zadania\\liczby.txt").getLines.toList.
    map((s: String) => s.toInt)
  val result = (numbers.filter((n: Int) => n%2==0), numbers.filter((n: Int) => n%2 != 0))

  //Zadanie 4
  val instances: Int = Source.fromFile("src\\laboratoria\\zadania\\cyfry.txt").getLines.toList.
    map((s: String) => s.toList.map((c: Char) => (c + "").toInt)).foldLeft(0)((i: Int, seq: Seq[Int]) => i + f(seq)())

  def f(seq: Seq[Int], j: Int = 0)(isSeq: Boolean = false): Int = seq match {
    case Seq() => j
    case e1 +: e2 +: s if e1 < e2 && isSeq => f(e2 +: s, j)(isSeq = true)
    case e1 +: e2 +: s if e1 < e2 => f(e2 +: s, j+1)(isSeq = true)
    case _ +: s if isSeq => f(s, j)(isSeq = false)
    case _ +: s => f(s, j)()
  }

}
