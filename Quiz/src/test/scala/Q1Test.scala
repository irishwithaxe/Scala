import org.scalatest.{FunSuite, Matchers}
import Q1._

class Q1Test extends FunSuite {

  test("pointer test") {
    val pointer = Pointer(3)
    assert(pointer.next.value == 1)

    val epointer = Pointer(2)
    assert(epointer.arrayLength == 2)
    assert(epointer.next.value == 1)
    assert(epointer.next.next.value == 0)
    assert(epointer.next.next.next.value == 1)
  }

  test("Q1. Generic queue of strings") {
    val queue = ArrayStringQueue(5)
    assert(queue.isEmpty)
    assert(!queue.isFull)

    queue.put("D")

    assert(!queue.isEmpty)
    assert(!queue.isFull)

    queue.put("D")
    queue.put("D")

    assert(!queue.isEmpty)
    assert(!queue.isFull)

    assert(queue.get === Some("D"))
    assert(queue.get === Some("D"))
    assert(queue.get === Some("D"))
    assert(queue.get === None)
    assert(queue.get === None)

    assert(queue put "A")
    assert(queue put "B")
    assert(queue put "C")
    assert(queue put "C")
    assert(queue put "C")

    assert(!queue.isEmpty)
    assert(queue.isFull)

    assert(!(queue put "C"))
    assert(!(queue put "C"))
    assert(!(queue put "C"))
    assert(!(queue put "C"))

    assert(!queue.isEmpty)
    assert(queue.isFull)

    assert(queue.get === Some("A"))
    assert(queue.get === Some("B"))
    assert(queue.get === Some("C"))
    assert(queue.get === Some("C"))
    assert(queue.get === Some("C"))

    assert(queue.isEmpty)
    assert(!queue.isFull)

    assert(queue.get === None)
    assert(queue.get === None)
  }
}
