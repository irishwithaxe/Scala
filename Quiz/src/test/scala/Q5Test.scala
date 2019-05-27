import org.scalatest.{FunSuite, Matchers}

import Q4.GCD
import Q5._

class Q5Test extends FunSuite {

  test("Q5. measure run time") {

    val started = System.nanoTime
    val measuredBlockResult = timeit {
      println("start of a code block")
      GCD(16, 15)
      GCD(14, 15)
      GCD(12, 15)
      GCD(10, 15)
      println("end of a code block")
      5
    }

    val ended = System.nanoTime
    val diff = ended - started

    println("the whole block finished in " + diff + " nano sec")

    assert(measuredBlockResult == 5)
  }
}
