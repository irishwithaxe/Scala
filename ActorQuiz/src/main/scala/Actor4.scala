import akka.actor.{Actor, ActorContext, ActorLogging, ActorRef, Props}

import scala.collection.mutable
import scala.util.Random

object Actor4 {
  /*
1.) Create a system with n agents (consisting of actors with unique ids from 1 to n). Furthermore
create one additional actor which we refer to as a “scheduler”. Messages are passed
between the agents and the scheduler, containing a time (between 0 and +inf). There are
two types of messages: 1.) Trigger 2.) Acknowledgement. Each agent receiving a Trigger
message from the scheduler and sends a new Trigger message to the scheduler (with time of
received trigger + random number between 0 and100. It also sends an Acknowledgement
message to the scheduler after that. The agent repeats such behavior for max 10 times
(meaning during the whole program it receives 10 Trigger messages from scheduler, sends 9
Trigger messages back to the Scheduler and sends 10 Acknowledgement messages to back
10 scheduler; there is one less Trigger message sent back, in order to eventually stop the
“simulation”).

2.) The scheduler upon receiving a Trigger messages stores the message and actor reference it in
a sorted Trigger queue (sorted by time). Whenever the Scheduler receives an
Acknowledgement message and there are still more Triggers in the sorted queue, it sends
out the first Trigger message to the corresponding agent actor.

3.) Each agent should write down in the console log, its id and Trigger message time whenever it
receives a new Trigger message from the scheduler.

4.) The scheduler is initialized with one Trigger message per agent (with random time between 0
and 100). After initialization a Start message is sent to the scheduler, which initiates the
“simulation”: The scheduler removes the first message in the sorted Trigger queue and sends
the first Trigger to the corresponding agent.
   */

  object Agent {
    def props(id: Int, maxCycles: Int): Props = Props(new Agent(id, maxCycles))

    final case class Trigger(time: Int, sender: ActorRef)
    final object Acknowledgement
  }

  class Agent(id: Int, maxCycles: Int) extends Actor with ActorLogging {
    private val random: Random = Random
    private var cyclesCnt = 0

    override def receive: Receive = {
      case Agent.Trigger(time, sender) =>
        log.info(s"$id: $time")

        cyclesCnt += 1
        if (cyclesCnt < maxCycles) {
          sender ! Agent.Trigger(time + random.nextInt(101), self)
        }
        if (cyclesCnt <= maxCycles) {
          sender ! Agent.Acknowledgement
        }
    }
  }

  trait AgentFactory {
    def make(actorContext: ActorContext, agentId: Int, maxCycles: Int): ActorRef
  }

  object Scheduler {
    final object DefaultAgentFactory extends AgentFactory {
      override def make(actorContext: ActorContext,
                        agentId: Int,
                        maxCycles: Int): ActorRef =
        actorContext.actorOf(Agent.props(agentId, maxCycles))
    }

    def props(agentsAmount: Int, maxCycles: Int = 10)(
        implicit agentFactory: AgentFactory = DefaultAgentFactory): Props =
      Props(new Scheduler(agentsAmount, maxCycles, agentFactory))

    final object Start
  }

  class Scheduler(agentsAmount: Int, maxCycles: Int, agentFactory: AgentFactory)
      extends Actor {
    val queue: mutable.PriorityQueue[Agent.Trigger] = {
      val emptyQueue =
        mutable.PriorityQueue.empty[Agent.Trigger](Ordering.by(-_.time))
      if (agentsAmount < 1) emptyQueue
      else {
        val random: Random = Random
        emptyQueue ++ (1 to agentsAmount).map(
          agentId =>
            Agent.Trigger(random.nextInt(101),
                          agentFactory.make(context, agentId, maxCycles)))
      }
    }

    override def receive: Receive = {
      case trigger: Agent.Trigger => queue enqueue trigger
      case Scheduler.Start | Agent.Acknowledgement =>
        if (queue.nonEmpty) {
          val trigger = queue.dequeue()
          trigger.sender ! Agent.Trigger(trigger.time, self)
        }
    }
  }
}
