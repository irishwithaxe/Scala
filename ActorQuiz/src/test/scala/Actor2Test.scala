import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import Actor2._
import Actor2.ChainItem._
import Actor2.ActorsChain._

import scala.concurrent.duration._

class Actor2Test(_system: ActorSystem)
    extends TestKit(_system)
    with Matchers
    with WordSpecLike
    with BeforeAndAfterAll {

  def this() = this(ActorSystem("actor-quiz"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  val probe = TestProbe()
  object testPrinter extends Printer {
    override def print(str: String): Unit = probe.ref ! str
  }

  def messagesAreCorrect(messages: Seq[Any], expectedLen: Int): Boolean = {
    val processed = messages.map(msg => msg.toString)
    processed.foldLeft(messages.length == expectedLen)((acc, m) =>
      acc && m.contains(", message received") && m.contains("actor"))
  }

  "a chainItem" should {
    "should not do anything if next path is incorrect" in {
      val actor1 = _system.actorOf(ChainItem.props(1, Some("23"), testPrinter))
      actor1 ! ChainMessage()

      probe.expectNoMessage(1.second)
    }

    "should print a message if no nextPath given" in {
      val actor1 = _system.actorOf(ChainItem.props(1, None, testPrinter))
      actor1 ! ChainMessage()

      val messages = probe.receiveN(1, 1.second)
      messagesAreCorrect(messages, 1) shouldBe true
    }

    "should send a message next" in {
      val actor1 = _system.actorOf(ChainItem.props(1, Some("../2")), "1")
      _system.actorOf(ChainItem.props(2, Some("../3")), "2")
      _system.actorOf(ChainItem.props(3, Some("../4")), "3")
      _system.actorOf(ChainItem.props(4, Some("../5")), "4")
      _system.actorOf(ChainItem.props(5, None, testPrinter), "5")
      actor1 ! ChainMessage()

      val messages = probe.receiveN(5, 1.second)
      messagesAreCorrect(messages, 5) shouldBe true
    }
  }

  "a whole actors chain" should {
    "do nothing if chain length less than 1" in {
      val actorsChain1 = _system.actorOf(ActorsChain.props(0, testPrinter))
      actorsChain1 ! Message()

      val actorsChain2 = _system.actorOf(ActorsChain.props(-2, testPrinter))
      actorsChain2 ! Message()

      val actorsChain3 = _system.actorOf(ActorsChain.props(-42, testPrinter))
      actorsChain3 ! Message()

      probe.expectNoMessage(1.second)
    }

    "print a message if length is 1 or more" in {
      val actorsChain1 = _system.actorOf(ActorsChain.props(1, testPrinter))
      actorsChain1 ! Message()
      val messages1 = probe.receiveN(1, 1.second)
      messagesAreCorrect(messages1, 1) shouldBe true

      val actorsChain2 = _system.actorOf(ActorsChain.props(3, testPrinter))
      actorsChain2 ! Message()
      val messages2 = probe.receiveN(3, 1.second)
      messagesAreCorrect(messages2, 3) shouldBe true

      val actorsChain3 = _system.actorOf(ActorsChain.props(42, testPrinter))
      actorsChain3 ! Message(1)
      val messages3 = probe.receiveN(42, 1.second)
      messagesAreCorrect(messages3, 42) shouldBe true
    }
  }
}
