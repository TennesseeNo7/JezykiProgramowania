package src.exercises.laboratories

import scala.io.Source

object Lab10 extends App {

  val names: Seq[String] = Source.fromFile("src\\exercises\\laboratories\\osoby.txt").getLines.toList

  println("Hello world")

}
