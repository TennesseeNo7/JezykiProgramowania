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
  val sequences: Seq[String] = Source.fromFile("src\\laboratoria\\zadania\\cyfry.txt").getLines.toList
  val instances = sequences.foldLeft(0)((n: Int, s: String) => {
    ???
  })


}
