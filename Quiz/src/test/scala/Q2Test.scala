import org.scalatest.{FunSuite, Matchers}
import Q2._

class Q2Test extends FunSuite {

  test("Q2. String queue with reverted strings") {
    val queue = ReverseIntQueue()
    assert(queue put "Dada")
    assert(queue.get === Some("adaD"))
    assert(queue.get === None)
    assert(queue.get === None)

    assert(queue put "abbA")
    assert(queue put "boB")
    assert(queue put "eeffoC")

    assert(queue.get === Some("Abba"))
    assert(queue.get === Some("Bob"))
    assert(queue.get === Some("Coffee"))

    assert(queue.get === None)
    assert(queue.get === None)

  }
}
