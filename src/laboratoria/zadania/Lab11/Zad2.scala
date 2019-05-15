package laboratoria.zadania.Lab11

import scala.io.Source

object Zad2 extends App {

  {
    val lines: Seq[String] = Source.fromFile("src\\laboratoria\\zadania\\Lab11\\ogniem_i_mieczem.txt").getLines.toList
    histogram(lines)
  }

  def histogram(lines: Seq[String]): Unit = {
    println(
    lines.map((s: String) => s.toLowerCase().filter((c: Char) => charFilter(c)))
      .foldLeft("")((s: String, n: String) => s + n).groupBy((c: Char) => c).mapValues((s: String) => s.size))
  }

  def charFilter(c: Char): Boolean = c match {
    case k if k > 96 && k < 123 || k == 'ą' || k == 'ć' || k == 'ę' || k == 'ł' || k == 'ń' || k == 'ó' || k == 'ś'
    || k == 'ź' || k == 'ż' => true
    case _ => false
  }



}
