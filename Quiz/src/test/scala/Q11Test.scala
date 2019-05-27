import org.scalatest.{FunSuite, Matchers}
import Q11._

class Q11Test extends FunSuite {
  test("Q11. first not None") {
    assert(FirstNotNone(None, Some(12), Some(42)) === Some(12))
    assert(FirstNotNone(None, None, Some(42)) === Some(42))
    assert(FirstNotNone(Some(1), Some(24), Some(42)) === Some(1))
    assert(FirstNotNone(None, None, None) === None)
  }

}
