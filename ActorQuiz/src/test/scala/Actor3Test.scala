import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import Actor3._

import scala.concurrent.duration._

class Actor3Test(_system: ActorSystem)
    extends TestKit(_system)
    with Matchers
    with WordSpecLike
    with BeforeAndAfterAll {

  def this() = this(ActorSystem("actor-quiz"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  val printer = TestProbe()
  object testPrinter extends Printer {
    override def print(str: String): Unit = {
      println("printer: " + str)
      printer.ref ! str
    }
  }

  "neighbour actor" should {
    "not do anything if paths are wrong" in {
      val n1 =
        _system.actorOf(
          Neighbour
            .props(1, "wrongLeftPath", "wrongRightPath", "{}, {}", testPrinter))
      n1 ! Neighbour.NeighbourMessage("text", "")

      printer.expectNoMessage(0.5.second)
    }

    "send message to left if got it from right" in {
      val leftN = TestProbe()
      val rightN = TestProbe()
      val n = _system.actorOf(
        Neighbour.props(1,
                        leftN.ref.path.toString,
                        rightN.ref.path.toString,
                        "",
                        testPrinter))

      leftN.send(
        n,
        Neighbour.NeighbourMessage("test message", leftN.ref.path.toString))
      rightN.expectMsg(
        Neighbour.NeighbourMessage("test message", n.path.toString))

      rightN.expectNoMessage(0.5.second)
      printer.expectNoMessage(0.5.second)
    }

    "send message to left and right if got it from not left nor right" in {
      val leftN = TestProbe()
      val rightN = TestProbe()
      val n = _system.actorOf(
        Neighbour.props(1,
                        leftN.ref.path.toString,
                        rightN.ref.path.toString,
                        "",
                        testPrinter))

      n ! Neighbour.NeighbourMessage("test message", "someParent")
      leftN.expectMsg(
        Neighbour.NeighbourMessage("test message", n.path.toString))
      rightN.expectMsg(
        Neighbour.NeighbourMessage("test message", n.path.toString))

      leftN.expectNoMessage(0.5.second)
      rightN.expectNoMessage(0.5.second)
      printer.expectNoMessage(0.5.second)
    }

    "send message to right if got it from left" in {
      val leftN = TestProbe()
      val rightN = TestProbe()
      val n = _system.actorOf(
        Neighbour.props(1,
                        leftN.ref.path.toString,
                        rightN.ref.path.toString,
                        "",
                        testPrinter))

      rightN.send(
        n,
        Neighbour.NeighbourMessage("test message 42", rightN.ref.path.toString))
      leftN.expectMsg(
        Neighbour.NeighbourMessage("test message 42", n.path.toString))

      leftN.expectNoMessage(0.5.second)
      printer.expectNoMessage(0.5.second)
    }

    "print a message after receiving it from both neighbours" in {
      val leftN = TestProbe()
      val rightN = TestProbe()
      val n = _system.actorOf(
        Neighbour.props(
          48,
          leftN.ref.path.toString,
          rightN.ref.path.toString,
          "Got message from both neighbours. Id: %d, message: '%s'",
          testPrinter))

      rightN.send(
        n,
        Neighbour.NeighbourMessage("test message 42", rightN.ref.path.toString))
      leftN.expectMsg(
        Neighbour.NeighbourMessage("test message 42", n.path.toString))
      leftN.send(
        n,
        Neighbour.NeighbourMessage("test message 42", leftN.ref.path.toString))

      leftN.expectNoMessage(0.5.seconds)
      rightN.expectNoMessage(0.5.seconds)

      printer.expectMsg(
        "Got message from both neighbours. Id: 48, message: 'test message 42'")
    }
  }

  "neighbourhood actor" should {
    "do nothing if neighbourhood size less than 1" in {
      val n1 = _system.actorOf(Neighbourhood.props(-100, "", testPrinter))
      n1 ! Neighbourhood.Message(1, "test message")

      val n2 = _system.actorOf(Neighbourhood.props(-1, "", testPrinter))
      n2 ! Neighbourhood.Message(1, "test message")

      val n3 = _system.actorOf(Neighbourhood.props(0, "", testPrinter))
      n3 ! Neighbourhood.Message(1, "test message")

      printer.expectNoMessage(0.5.seconds)
    }

    "print 2 message if neighbourhood size is 1" in {
      val n1 =
        _system.actorOf(Neighbourhood.props(1, "Got message from both neighbours. Id: %s message: %s", testPrinter))
      n1 ! Neighbourhood.Message(1, "test message to 1")

      printer.receiveN(2)
      printer.expectNoMessage(0.5.seconds)
    }

    "print 2 message if neighbourhood size is 2" in {
      val n1 =
        _system.actorOf(Neighbourhood.props(2, "Got message from both neighbours. Id: %s message: %s", testPrinter))
      n1 ! Neighbourhood.Message(1, "test message to 1")

      printer.receiveN(2)
      printer.expectNoMessage(0.5.seconds)
    }

    "print 2 messages if neighbourhood size more than 2" in {
      val n1 =
        _system.actorOf(Neighbourhood.props(3, "Got message from both neighbours. Id: %s message: %s", testPrinter),
                        "neighbourhood3")
      n1 ! Neighbourhood.Message(2, "test message to 2")

      printer.receiveN(2)
      printer.expectNoMessage(0.5.seconds)
    }

    "print 2 messages if neighbourhood size is 42" in {
      val n2 = _system.actorOf(
        Neighbourhood.props(
          42,
          "Got message from both neighbours. Id: %s message: %s",
          testPrinter))
      val messageText = "'hi!' from thirteenth"
      n2 ! Neighbourhood.Message(13, messageText)

      val messages = printer.receiveN(2)
      printer.expectNoMessage(1.seconds)

      messages.foldLeft(true)((aggregator, message) => {
        val m = message.toString
        m.contains("Got message from both neighbours.") &&
        m.contains(messageText) &&
        aggregator
      }) shouldBe true
    }

    "print 2 messages if neighbourhood size is 25" in {
      val n2 = _system.actorOf(
        Neighbourhood.props(
          25,
          "Got message from both neighbours. Id: %d message: %s",
          testPrinter))
      val messageText = "'hi!' from thirteenth"
      n2 ! Neighbourhood.Message(13, messageText)

      val messages = printer.receiveN(2)
      printer.expectNoMessage(1.seconds)

      messages.foldLeft(true)((aggregator, message) => {
        val m = message.toString
        m.contains("Got message from both neighbours.") &&
        m.contains(messageText) &&
        aggregator
      }) shouldBe true
    }
  }
}
