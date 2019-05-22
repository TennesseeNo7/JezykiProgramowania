package laboratoria.zadania.Lab12

import akka.actor.{Actor, ActorRef, ActorSystem, Kill, PoisonPill, Props}

import scala.io.Source

object Zad1 extends App {

  val system = ActorSystem()

  case class Init(numberOfWorkers: Int)

  case class Task(text: List[String])

  case class Execute(text: String)

  case class Result(res: Int)

  // WORKER
  class Worker extends Actor {

    override def receive: PartialFunction[Any, Unit] = {
      case Execute(text) =>
        sender ! Result(text.foldLeft(0)((i: Int, c: Char) => if(c == ' ') i+1 else i) + 1)
    }

  }

  // COORDINATOR
  class Coordinator extends Actor {

    var workers: Seq[ActorRef] = Seq()
    var initialized = false
    var pointer = 0
    var actorsCount: Int = 0

    var finalResult = 0
    var executed = 0
    var toBeExecuted = 0

    override def receive: PartialFunction[Any, Unit] = {
      case Init(numberOfWorkers) =>
        var x = 0; while(x < numberOfWorkers) {
          workers = workers :+ ActorSystem().actorOf(Props[Worker], name="worker" + x)
        x += 1
        }
        initialized = true
        actorsCount = numberOfWorkers
      case Task(text) if initialized =>

        toBeExecuted = text.size

        def assign(textList: List[String] = text): Unit = textList match {
          case _ if pointer == actorsCount =>
            pointer = 0
            assign(textList)
          case List(s) =>
            workers(pointer) ! Execute(s)
            pointer = 0
          case e +: r =>
            workers(pointer) ! Execute(e)
            pointer += 1
            assign(r)
        }

        assign(text)

      case Result(res) =>
        finalResult += res
        executed += 1
        //println(res + " " + executed + " " + toBeExecuted)
        if(executed == toBeExecuted) {
          println(finalResult)
          // TODO Terminate system
        }

    }

  }

  def dane(): List[String] = {
    Source.fromFile("src\\laboratoria\\zadania\\Lab11\\ogniem_i_mieczem.txt").getLines.toList
  }

  {
    val coordinator: ActorRef = system.actorOf(Props[Coordinator], "client")

    coordinator ! Init(2)
    coordinator ! Task(dane())
  }

}


