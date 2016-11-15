import akka.actor.Actor.emptyBehavior
import akka.actor.{Actor, ActorSystem, Props}

object Main {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("HelloWorld")
    val worker = system.actorOf(Props[Worker], name = "worker")
    worker ! Hello
    system terminate()
  }

  class Worker extends Actor {
    override def receive: Receive = {
      case Hello => println("Hello, world!")

        context stop self
        emptyBehavior
    }
  }

  sealed trait Msg

  case object Hello extends Msg

}