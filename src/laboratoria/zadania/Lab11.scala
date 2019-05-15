package laboratoria.zadania

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object Lab11 extends App {

  case class Play(player2: ActorRef)

  case object Pong

  case object Ping

  class Computer extends Actor {

    var count = 10

    def receive: PartialFunction[Any, Unit] = {
      case Play(p) =>
        println("Let's start")
        p ! Ping
        count -= 1
      case Pong =>
        Thread.sleep(400)
        if(count > 0) {
          println("Tak jak pan jezus powiedzial")
          sender ! Ping
          count -= 1
        } else {
          println("Nie wiem")
          system.terminate()
        }
      case Ping =>
        Thread.sleep(400)
        if(count > 0) {
          println("No a jak pan Jeus powiedzial?")
          sender ! Pong
          count -= 1
        } else {
          println("Nie wiem")
          system.terminate()
        }

    }

  }

  val system = ActorSystem()
  val player1 = system.actorOf(Props[Computer], "ping")
  val player2 = system.actorOf(Props[Computer], "pong")

  player1 ! Play(player2)

}
