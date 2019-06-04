import akka.actor.{Actor, ActorRef, Props}

import scala.concurrent.duration._
import scala.util.Random

object Actor3 {
  /*
1.) Create an n‐actor system (unique ids from 1 to n), where each actor i can only send and
receive messages to/from actor i‐1 and i+1 (actor n can communicate with actor n‐1 and
actor 0).
2.) In the main program, we send a String message (“hello from: x ”) to one random actor x. That
random actor sends the message to its two neighbors. Each neighbor receiving the message
just forwards it to that neighbor which did not send the message to it, after introducing a
random delay between 1 and 100ms.
3.) When an actor has received a message from both of its neighbors, it prints out its own Id and
the message it received
   */

  trait Printer {
    def print(str: String)
  }

  final object DefaultPrinter extends Printer {
    override def print(str: String): Unit = println(str)
  }

  object Neighbour {
    def props(id: Int,
              leftPath: String,
              rightPath: String,
              finalMessage: String,
              printer: Printer): Props =
      Props(new Neighbour(id, leftPath, rightPath, finalMessage, printer))

    final case class NeighbourMessage(text: String, senderPath: String)
  }

  class Neighbour(id: Int,
                  leftPath: String,
                  rightPath: String,
                  finalMessage: String,
                  printer: Printer)
      extends Actor {
    import Neighbour._

    private var getFromLeft: Boolean = false
    private var getFromRight: Boolean = false
    private val random: Random = Random

    private def sendWithDelay(message: String,
                              path: String,
                              selfPath: String): Unit = {
      import context._
      val delay = (random.nextInt(100) + 1).milliseconds
      context.system.scheduler.scheduleOnce(delay) {
        context.actorSelection(path) !
          NeighbourMessage(message, selfPath)
      }
    }

    override def receive: Receive = {
      case message: NeighbourMessage =>
        if (message.senderPath == leftPath) getFromLeft = true
        if (message.senderPath == rightPath) getFromRight = true

        if (!getFromLeft) {
          sendWithDelay(message.text, leftPath, self.path.toString)
        }
        if (!getFromRight) {
          sendWithDelay(message.text, rightPath, self.path.toString)
        }
        if (getFromLeft && getFromRight) {
          printer.print(finalMessage.format(id, message.text))
        }

    }
  }

  object Neighbourhood {
    def props(neighboursAmount: Int, finalMessage:String, printer: Printer = DefaultPrinter): Props =
      Props(new Neighbourhood(neighboursAmount, finalMessage, printer))

    final case class Message(recipient: Int, message: String)
  }

  class Neighbourhood(neighboursAmount: Int, finalMessage:String, printer: Printer) extends Actor {
    val neighbours: Map[Int, ActorRef] = {
      if (neighboursAmount < 3) Map[Int, ActorRef]()
      else {
        def makeNeighbour(id: Int, leftId: Int, rightId: Int): ActorRef = {
          val lp = self.path + "/" + leftId.toString
          val rp = self.path + "/" + rightId.toString
          context.actorOf(Neighbour.props(id, lp, rp, finalMessage, printer), id.toString)
        }

        val first = makeNeighbour(1, neighboursAmount, 2)
        val last = makeNeighbour(neighboursAmount, neighboursAmount - 1, 1)

        Map[Int, ActorRef](1 -> first) + (neighboursAmount -> last) ++
          (2 until neighboursAmount).map(id =>
            id -> makeNeighbour(id, id - 1, id + 1))
      }
    }

    override def receive: Receive = {
      case Neighbourhood.Message(recipient, message) =>
        neighbours get recipient match {
          case Some(neighbour) =>
            neighbour ! Neighbour.NeighbourMessage(message, self.path.toString)
          case _ =>
        }
    }
  }
}
