import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, _}
import scala.util.Random

object Main {
  val peopleCount = 5

  lazy val system: ActorSystem = ActorSystem("CodingDevelopers")
  lazy val waiter: ActorRef = createWaiter
  lazy val developers: Seq[ActorRef] = 0 until 5 map createDeveloper

  def main(args: Array[String]): Unit = {
    developers.par.foreach(dev => dev ! Start)

    // we'll assume that after 1 minute the coding day is over
    developers.par.foreach(dev => dev !!! Stop)
    waiter !!! Stop
  }

  implicit class ActorRefOps(actor: ActorRef) {

    def !!(message: Msg): Unit = system.scheduler.scheduleOnce(randomTime, actor, message)

    def !!!(message: Msg): Unit = system.scheduler.scheduleOnce(1 minute, actor, message)

    def info(string: String): Unit = {
      val name = actor.path.name
      println(s"$name: $string")
    }
  }

  def randomTime: FiniteDuration = Random nextInt 5000 millis

  def createWaiter: ActorRef = system.actorOf(Props[Waiter], name = "Waiter")

  def createDeveloper(index: Int): ActorRef =
    system.actorOf(Props(classOf[Developer], index), name = "Developer" + index)

  class Developer(index: Int) extends Actor {
    override def receive: Receive = {
      case Start =>
        self info "Starting a nice day of coding"
        waiter !! WannaBailOutAndEat(index)
      case PleaseEat =>
        self info "OK, time to eat! Eating.."
        self !! WannaCodeAgain
      case WannaCodeAgain =>
        self info "One hell of a meal! Coding again.."
        waiter ! StoppedEating(index)
        self !! StopCoding
      case TheBuildIsBroken =>
        self info "The build is broken. No time to eat! Coding.."
        self !! StopCoding
      case StopCoding =>
        self info "Yawn. I'm hungry! Can I eat?"
        waiter ! WannaBailOutAndEat(index)
      case Stop =>
        self info "Oops. No tasks closed today. Will work on them tomorrow."
        context stop self
    }
  }

  class Waiter extends Actor {
    var forks = Vector.fill(peopleCount)(true)

    override def receive: Receive = {
      case WannaBailOutAndEat(index) =>
        if (isForksAvailable(index)) prepareForksAndGiveTheFood(index) else noFood(index)
      case StoppedEating(index) =>
        freeTheForks(index)
      case Stop =>
        self info "Time to go home and watch a movie. I'll shut down the system."
        context stop self
        system terminate
    }

    def isForksAvailable(index: Int): Boolean = {
      forks(index) && forks((index + 1) % peopleCount)
    }

    def freeTheForks(index: Int): Unit = {
      updateForks(index, value = true)
    }

    def prepareForksAndGiveTheFood(index: Int): Unit = {
      updateForks(index, value = false)
      self info s"Developer$index, here's your tasty burger!"
      developers(index) ! PleaseEat
    }

    def updateForks(index: Int, value: Boolean): Unit = {
      forks = forks updated(index, value) updated((index + 1) % peopleCount, value)
    }

    def noFood(index: Int): Unit = {
      self info s"Developer$index, check the Jenkins server!"
      developers(index) ! TheBuildIsBroken
    }
  }

  sealed trait Msg

  case class WannaBailOutAndEat(index: Int) extends Msg

  case class StoppedEating(index: Int) extends Msg

  case object WannaCodeAgain extends Msg

  case object PleaseEat extends Msg

  case object TheBuildIsBroken extends Msg

  case object Stop extends Msg

  case object StopCoding extends Msg

  case object Start extends Msg

}