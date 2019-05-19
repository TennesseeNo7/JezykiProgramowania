package laboratoria.zadania.Lab11

import scala.io.Source

object Zad2 extends App {

  {
    val lines: Seq[String] = Source.fromFile("src\\laboratoria\\zadania\\Lab11\\ogniem_i_mieczem.txt").getLines.toList
    histogram(lines, 40)
  }

  def histogram(lines: Seq[String], maxWidth: Int): Unit = {

    def charFilter(c: Char): Boolean = c match {
      case k if k > 96 && k < 123 || k == 'ą' || k == 'ć' || k == 'ę' || k == 'ł' || k == 'ń' || k == 'ó' || k == 'ś'
        || k == 'ź' || k == 'ż' => true
      case _ => false
    }

    val histMap: Map[Char, Int] = lines.map((s: String) => s.toLowerCase().filter((c: Char) => charFilter(c)))
      .foldLeft("")((s: String, n: String) => s + n).groupBy((c: Char) => c).mapValues((s: String) => s.length)
    val max = histMap.values.max

    val getWidth = (n: Int) => (maxWidth*n)/max

    val histList = histMap.toSeq.sortWith( (e1: (Char, Int), e2: (Char, Int)) => e1 match {
      case (c, _) if c < e2._1 => true
      case _ => false
    })

    def printHistogram(histList: Seq[(Char, Int)]): Unit = histList match {
      case e +: s if getWidth(e._2) == 0 =>
        println(e._1 + ": " + e._2)
        printHistogram(s)
      case e +: s =>
        println(e._1 + ": " + "*"*getWidth(e._2) + " " + e._2)
        printHistogram(s)
      case Seq() => ()
    }

    printHistogram(histList)

  }

}
