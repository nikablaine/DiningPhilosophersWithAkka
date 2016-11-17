import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration

object Main {
  val RANDOM = scala.util.Random
  val PEOPLE_COUNT = 5

  var system: ActorSystem = null
  var waiter: ActorRef = null
  var developers: Vector[ActorRef] = Vector()

  def main(args: Array[String]): Unit = {
    system = ActorSystem("DiningDevelopers")
    Range(0, 5).foreach(int => developers = developers :+ createDeveloper(int))
    waiter = createWaiter

    developers.foreach(dev => dev ! Start)
    // system terminate()
  }

  def createWaiter: ActorRef = {
    system.actorOf(Props[Waiter], name = "waiter")
  }

  def createDeveloper(index: Int): ActorRef = {
    system.actorOf(Props(classOf[Developer], index), name = "developer" + index)
  }


  class Developer(index: Int) extends Actor {
    override def receive: Receive = {
      case Start =>
        info("Starting a nice day of coding")
        !!(waiter, WannaBailOutAndEat(index))
      case PleaseEat =>
        info("OK, time to eat! Eating..")
        !!(self, WannaCodeAgain)
      case WannaCodeAgain =>
        info("One hell of a meal! Coding again..")
        waiter ! StoppedEating(index)
        !!(self, StopCoding)
      case TheBuildIsBroken =>
        info("The build is broken. No time to eat! Coding..")
        !!(self, StopCoding)
      case StopCoding =>
        info("Yawn. I'm hungry!")
        waiter ! WannaBailOutAndEat(index)
      case Stop =>
        info("Stopping..")
        context stop self
    }

    var developerLine: String = "Developer" + index + ": "

    def info(string: String): Unit = println(developerLine + string)

    def !!(actor: ActorRef, message: Msg): Unit = {
      system.scheduler.scheduleOnce(randomTime, actor, message)
    }

    def randomTime: FiniteDuration = {
      FiniteDuration(RANDOM.nextInt(5000), TimeUnit.MILLISECONDS)
    }
  }

  class Waiter extends Actor {
    var forks = Vector.fill(5)(true)

    override def receive: Receive = {
      case WannaBailOutAndEat(index) =>
        if (isForksAvailable(index)) prepareForksAndGiveTheFood(index) else noFood(index)
      case StoppedEating(index) =>
        freeTheForks(index)
    }

    def isForksAvailable(index: Int): Boolean = {
      forks(index) && forks((index + 1) % 5)
    }

    def freeTheForks(index: Int): Unit = {
      updateForks(index, true)
    }

    def prepareForksAndGiveTheFood(index: Int): Unit = {
      updateForks(index, false)
      developers(index) ! PleaseEat
    }

    def updateForks(index: Int, value: Boolean): Unit = {
      forks = forks updated(index, value)
      forks = forks updated((index + 1) % PEOPLE_COUNT, value)
    }

    def noFood(index: Int): Unit = {
      developers(index) ! TheBuildIsBroken
    }

    def info(string: String): Unit = println("Waiter: " + string)

  }

  sealed trait Msg

  case class WannaBailOutAndEat(index: Int) extends Msg

  case class StoppedEating(index: Int) extends Msg

  case object WannaCodeAgain extends Msg

  case object GotACodingIdea extends Msg

  case object PleaseEat extends Msg

  case object TheBuildIsBroken extends Msg

  case object Stop extends Msg

  case object StopCoding extends Msg

  case object Start extends Msg

}