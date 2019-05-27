import org.scalatest.{FunSuite, Matchers}
import Q12._

class Q12Test extends FunSuite with Matchers {
  test("Q12. multiplying corresponding numbers from list and map") {
    val list = List(1, 2, 3, 4)
    val map = Map[Int, Double](1 -> 3, 3 -> 5)

    MultCorrespondings(list, map) shouldEqual List(3.0, 15.0)
  }
}
