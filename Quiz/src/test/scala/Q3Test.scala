import org.scalatest.{FunSuite, Matchers}
import Q3._

class Q3Test extends FunSuite {
  test("Double equality operator") {
    assert(34.00000001 ~= 34.0)
    assert(!34.00000001.~=(34.0, allowedDifference = 0.00000000001))
  }
}
