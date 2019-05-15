package laboratoria.zadania.Lab11

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object Zad1 extends App {

  val system = ActorSystem()
  val player1 = system.actorOf(Props[Coordinator], "ping")
  val player2 = system.actorOf(Props[Coordinator], "pong")

  player1 ! Play(player2)

}

object Play {
  case class Play(player2: ActorRef)
  case object Pong
  case object Ping
}

class Coordinator() extends Actor {

  import Play._

  var counter = 10

  def receive: PartialFunction[Any, Unit] = {
    case Play(p) =>
      println("Let's start")
      p ! Ping
      counter -= 1
    case Pong =>
      Thread.sleep(400)
      if(counter > 0) {
        println("Pong")
        sender ! Ping
        counter -= 1
      } else {
        println("Nie wiem")
        ActorSystem().terminate()
      }
    case Ping =>
      Thread.sleep(400)
      if(counter > 0) {
        println("Ping")
        sender ! Pong
        counter -= 1
      } else {
        println("The end")
        ActorSystem().terminate()
      }

  }

}
