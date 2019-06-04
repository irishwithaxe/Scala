import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import Actor1._
import Actor1.ActorsChain._
import Actor1.ChainItem._

import scala.concurrent.duration._

class Actor1Test(_system: ActorSystem)
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

  "a chainItem" should {
    "should not do anything if next path is incorrect" in {
      val actor1 = _system.actorOf(ChainItem.props(1, Some("23"), testPrinter))
      actor1 ! ChainMessage(42)

      probe.expectNoMessage(1.second)

    }

    "should print a message if no nextPath given" in {
      val actor1 = _system.actorOf(ChainItem.props(1, None, testPrinter))
      actor1 ! ChainMessage(42)

      probe.expectMsg("Got message with numberOfHopsTravelled value 42")
    }

    "should send a message next" in {
      val actor1 = _system.actorOf(ChainItem.props(1, Some("../2")), "1")
      _system.actorOf(ChainItem.props(2, Some("../3")), "2")
      _system.actorOf(ChainItem.props(3, Some("../4")), "3")
      _system.actorOf(ChainItem.props(4, Some("../5")), "4")
      _system.actorOf(ChainItem.props(5, None, testPrinter), "5")
      actor1 ! ChainMessage()

      probe.expectMsg("Got message with numberOfHopsTravelled value 4")
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
      probe.expectMsg("Got message with numberOfHopsTravelled value 0")

      val actorsChain2 = _system.actorOf(ActorsChain.props(3, testPrinter))
      actorsChain2 ! Message()
      probe.expectMsg("Got message with numberOfHopsTravelled value 2")

      val actorsChain3 = _system.actorOf(ActorsChain.props(42, testPrinter))
      actorsChain3 ! Message(1)
      probe.expectMsg("Got message with numberOfHopsTravelled value 42")
    }

  }
}
