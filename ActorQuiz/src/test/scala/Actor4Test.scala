import akka.actor.{ActorContext, ActorRef, ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import Actor4._

import scala.concurrent.duration._

class Actor4Test(_system: ActorSystem)
    extends TestKit(_system)
    with Matchers
    with WordSpecLike
    with BeforeAndAfterAll {

  def this() = this(ActorSystem("actor-quiz"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  class TestAgentFactory(messageFormat: String = "+triggered")
      extends AgentFactory {
    val accumulator: TestProbe = TestProbe()

    class TestAgent(id: Int,
                    maxCycles: Int,
                    val accum: ActorRef,
                    messageFormat: String)
        extends Agent(id, maxCycles) {
      override def receive: Receive = {
        case m: Agent.Trigger =>
          accum ! messageFormat.format(m.time, id)
          super.receive(m)
        case _ => println("+got a message")
      }
    }

    override def make(actorContext: ActorContext,
                      agentId: Int,
                      maxCycles: Int): ActorRef = {
      accumulator.ref ! "+agent"
      actorContext.actorOf(
        Props(
          new TestAgent(agentId, maxCycles, accumulator.ref, messageFormat)))
    }

    def props(agentId: Int, maxCycles: Int): Props =
      Props(new TestAgent(agentId, maxCycles, accumulator.ref, messageFormat))

  }

  "an agent actor" should {
    "send back Trigger and Acknowledgement" in {
      val agent1 = _system.actorOf(Agent.props(1, 100))
      val probe = TestProbe()

      agent1 ! Agent.Trigger(10, probe.ref)
      val messages = probe.receiveN(2)
      probe.expectNoMessage(0.5.seconds)

      messages.head shouldBe a[Agent.Trigger]
      messages.head.asInstanceOf[Agent.Trigger].sender shouldBe agent1

      messages(1) shouldBe Agent.Acknowledgement
    }

    "send only Acknowledgement when maxCycles-1 reached" in {
      val agent1 = _system.actorOf(Agent.props(1, 1))
      val probe = TestProbe()

      agent1 ! Agent.Trigger(42, probe.ref)
      probe.expectMsg(Agent.Acknowledgement)
      probe.expectNoMessage(0.5.seconds)
    }

    "send nothing after maxCycles reached" in {
      val agent1 = _system.actorOf(Agent.props(1, 1))
      val probe = TestProbe()

      agent1 ! Agent.Trigger(10, probe.ref)
      agent1 ! Agent.Trigger(11, probe.ref)
      agent1 ! Agent.Trigger(12, probe.ref)
      agent1 ! Agent.Trigger(13, probe.ref)
      agent1 ! Agent.Trigger(14, probe.ref)

      probe.expectMsg(Agent.Acknowledgement)
      probe.expectNoMessage(1.seconds)
    }
  }

  "a scheduler" should {
    "create an empty queue if agents count less than 1" in {
      implicit val testFactory: TestAgentFactory = new TestAgentFactory()
      _system.actorOf(Scheduler.props(0))
      _system.actorOf(Scheduler.props(-1))

      testFactory.accumulator.expectNoMessage(0.5.seconds)
    }

    "create an 2 agents if agents count 2" in {
      implicit val testFactory: TestAgentFactory = new TestAgentFactory()
      val scheduler = _system.actorOf(Scheduler.props(2))

      testFactory.accumulator
        .receiveN(2)
        .foreach(message => message shouldBe "+agent")
    }

    "get triggered agentsAmount*(maxCycles - 1) with sorted times" in {
      implicit val testFactory: TestAgentFactory = new TestAgentFactory("%d")
      val agentsAmount = 42
      val maxCycles = 42
      val scheduler = _system.actorOf(Scheduler.props(agentsAmount, maxCycles))

      testFactory.accumulator
        .receiveN(agentsAmount)
        .foreach(message => message shouldBe "+agent")

      scheduler ! Scheduler.Start
      testFactory.accumulator
        .receiveN(agentsAmount * (maxCycles - 1))
        .foldLeft(0)((acc, m) => {
          val v = m.toString.toInt
          acc should be <= v
          v
        })
    }
  }
}
