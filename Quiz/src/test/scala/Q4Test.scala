import org.scalatest.{FunSuite, Matchers}
import Q4._

class Q4Test extends FunSuite {
  test("Q4. gcd") {
    assert(GCD(0, 1) == 1)
    assert(GCD(0, 0) == 0)
    assert(GCD(100500, 0) == 100500)

    assert(GCD(4, 6) == 2)
    assert(GCD(2, 3) == 1)
    assert(GCD(2, 1) == 1)

    assert(GCD(2 * 2 * 3 * 5 * 7, 2 * 3 * 7 * 11) == 2 * 3 * 7)
    assert(GCD(3 * 5 * 7 * 23 * 79, 11 * 13 * 17 * 71 * 73) == 1)
  }
}
