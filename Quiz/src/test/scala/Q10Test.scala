import org.scalatest.{FunSuite, Matchers}
import Q10._

class Q10Test extends FunSuite with Matchers {
  test("Q10. squareList") {
    squareList(List(1, 1, 2, 4, 2, 10)) shouldEqual List(1, 1, 4, 16, 4, 100)
    squareList(List(10)) shouldEqual List(100)
    squareList(List()) shouldEqual List()
  }

  test("Q10. squareMapList") {
    squareMapList(List(1, 1, 2, 4, 2, 10)) shouldEqual List(1, 1, 4, 16, 4, 100)
    squareMapList(List(10)) shouldEqual List(100)
    squareMapList(List()) shouldEqual List()
  }
}
