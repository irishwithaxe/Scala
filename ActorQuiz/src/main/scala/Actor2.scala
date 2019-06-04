import akka.actor.{Actor, ActorRef, Props}

object Actor2 {
  /*
Adapt task 1, so that we can measure the messaging time between each actor. However, the
messaging time should still only be printed out by the last actor (n). The output should be in
the following format:
actor 1, message received <time‐in‐milli‐seconds>
actor 2, message received <time‐in‐milli‐seconds>
…
Actor n, message received <time‐in‐milli‐seconds>
Hint: You are allowed to adapt/extend the messages being sent among the actors to achieve
this
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

    final case class Measure(actorId: Int, time: Long)
    final case class ChainMessage(measures: Seq[Measure] = Seq[Measure]())
  }

  class ChainItem(id: Int, nextPath: Option[String], printer: Printer)
      extends Actor {
    import ChainItem._

    override def receive: Receive = {
      case ChainMessage(measures) =>
        val measure = Measure(id, System.currentTimeMillis())
        val newMeasures = measures :+ measure
        nextPath match {
          case Some(path) =>
            context.actorSelection(path) ! ChainMessage(newMeasures)
          case None =>
            newMeasures.foreach(measure =>
              printer.print(
                s"actor ${measure.actorId}, message received ${measure.time}"))
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

    override def preStart(): Unit = println("started")
    override def postStop(): Unit = println("stopped")

    override def receive: Receive = {
      case Message(number) =>
        first match {
          case Some(actorRef) => actorRef ! ChainMessage(Seq[Measure]())
          case None           =>
        }
    }
  }
}
