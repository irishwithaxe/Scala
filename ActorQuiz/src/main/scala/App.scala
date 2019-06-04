import akka.actor.ActorSystem

import scala.io.StdIn

object App {

  def runActor1App(system: ActorSystem): Unit = {
    import Actor1.ActorsChain
    import Actor1.ActorsChain.Message

    println("Task #1")
    println("enter amount of actors")
    val amount = StdIn.readInt()

    val master = system.actorOf(ActorsChain.props(amount))
    master ! Message()
  }

  def runActor2App(system: ActorSystem): Unit = {
    import Actor2.ActorsChain
    import Actor2.ActorsChain.Message

    println("Task #2")
    println("enter amount of actors")
    val amount = StdIn.readInt()

    val master = system.actorOf(ActorsChain.props(amount))
    master ! Message()
  }

  def runActor3App(system: ActorSystem): Unit = {
    import Actor3.Neighbourhood
    import Actor3.Neighbourhood.Message

    println("Task #3")
    println("enter amount of neighbours")
    val amount = StdIn.readInt()

    if (amount < 3) println("not enough neighbours!")
    else {
      val neighbourhood = system.actorOf(Neighbourhood.props(amount, "%d %s"))

      println("enter a number of a neighbour to send 'hello' message")
      val number = StdIn.readInt()

      if (number < 1 || number > amount) println("there is no such neighbour!")
      else neighbourhood ! Message(number, "hello from: " + number.toString)
    }
  }

  def runActor4App(system: ActorSystem): Unit = {
    import Actor4._

    println("Task #4")
    println("enter amount of agents")
    val amount = StdIn.readInt()

    if (amount < 1) println("not enough agents")
    else {
      val scheduler = system.actorOf(Scheduler.props(amount))
      scheduler ! Scheduler.Start
    }
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("chain-of-actors")

    try {
      println("enter task number")
      StdIn.readInt() match {
        case 1 => runActor1App(system)
        case 2 => runActor2App(system)
        case 3 => runActor3App(system)
        case 4 => runActor4App(system)
        case _ => println("wrong task number")
      }

      println("\npress ENTER to exit\n")
      StdIn.readLine()
    } finally {
      system.terminate()
    }
  }
}
