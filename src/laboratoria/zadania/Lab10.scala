package laboratoria.zadania

import scala.io.Source

object Lab10 extends App {

  val names: Seq[String] = Source.fromFile("src\\laboratoria\\zadania\\osoby.txt").getLines.toList

  println("Hello world")

}
