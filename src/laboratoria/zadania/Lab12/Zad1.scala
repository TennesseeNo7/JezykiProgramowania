package laboratoria.zadania.Lab12

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object Zad1 extends App {

  case class Send(a: Double, b: Double, c: Double, server: ActorRef)
  case class Verify(a: Double, b: Double, c: Double)

  class Client extends Actor {

    override def receive: PartialFunction[Any, Unit] = {

      case Send(a, b, c, s) =>
        s ! Verify(a, b, c)

      case b: Boolean =>
        if(b) {
          println("Z tych wartosci mozna utworzyc trojkat")
        } else {
          println("Z tych wartosci nie mozna utworzyc trojkata")
        }
        system.terminate()
    }

  }

  class Server extends Actor {

    override def receive: PartialFunction[Any, Unit] = {

      case Verify(a, b, c) if a+b > c => sender ! true
      case _ => sender ! true

    }

  }

  val system = ActorSystem()
  val client: ActorRef = system.actorOf(Props[Client], "client")
  val server: ActorRef = system.actorOf(Props[Server], "server")

  client ! Send(2, 3, 4, server)

}
