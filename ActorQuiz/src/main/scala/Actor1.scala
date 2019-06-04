import akka.actor.{Actor, ActorRef, Props}

object Actor1 {
  /*
1.) Familiarize yourself with the Akka Actor Model.

2.) After familiarizing yourself with the Akka Actor Model, build an application with n‐actors (n
can should be specifiable as input to the application). Each actor should be assigned an id
between 1 to n (unique). In the main program a message should be sent to the actor with id 1.
The message object should contain a field numberOfHopsTravelled (int, initialized to 0).

3.) Actor 1 forwards the message to the actor with id 2 when receiving the message from actor 1,
but before doing so, increases the numberOfHopsTravelled by one in the message. This
continues in this way until the message reaches actor n. When actor n receives the message,
it prints out the numberOfHopsTravelled field in the received message.
   */

  trait Printer {
    def print(str: String)
  }

  object DefaultPrinter extends Printer {
    override def print(str: String): Unit = println(str)
  }

  object ChainItem {
    def props(id: Int,
              nextName: Option[String],
              printer: Printer = DefaultPrinter): Props =
      Props(new ChainItem(id, nextName, printer))

    final case class ChainMessage(numberOfHopsTravelled: Int = 0)
  }

  class ChainItem(id: Int, nextPath: Option[String], printer: Printer)
      extends Actor {
    import ChainItem._

    override def receive: Receive = {
      case ChainMessage(numberOfHopsTravelled) =>
        nextPath match {
          case Some(path) =>
            context.actorSelection(path) ! ChainMessage(
              numberOfHopsTravelled + 1)
          case None =>
            printer.print(
              "Got message with numberOfHopsTravelled value " + numberOfHopsTravelled)

        }
    }
  }

  object ActorsChain {
    def props(chainLength: Int, printer: Printer = DefaultPrinter): Props =
      Props(new ActorsChain(chainLength, printer))
    final case class Message(startNumber: Int = 0)
  }

  class ActorsChain(chainLength: Int, printer: Printer) extends Actor {
    import ActorsChain._
    import ChainItem._

    val first: Option[ActorRef] = init(chainLength)

    private def init(chainLength: Int): Option[ActorRef] = {
      def createChainItem(id: Int, hasNext: Boolean): ActorRef =
        if (hasNext) {
          val nextPath = Some(self.path + "/" + (id + 1))
          context.actorOf(ChainItem.props(id, nextPath, printer), id.toString)
        } else
          context.actorOf(ChainItem.props(id, None, printer), id.toString)

      if (chainLength == 1) {
        Some(createChainItem(1, hasNext = false))
      } else if (chainLength > 1) {
        (2 until chainLength).foreach(createChainItem(_, hasNext = true))
        createChainItem(chainLength, hasNext = false)
        Some(createChainItem(1, hasNext = true))
      } else {
        None
      }
    }

    override def receive: Receive = {
      case Message(number) =>
        first match {
          case Some(actorRef) => actorRef ! ChainMessage(number)
          case None           =>
        }
    }
  }
}
