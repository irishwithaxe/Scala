import org.scalatest.{FunSuite, Matchers}
import Q13._

class Q13Test extends FunSuite with Matchers {
  test("Q13. retry method with imported config") {
    var counter = 0
    import Q13.RetryThreeTimes._

    retry {
      counter += 1
    }

    counter shouldBe 3
  }
  test("Q13. retry method with default config") {
    var counter = 0

    retry {
      counter += 1
    }

    counter shouldBe 2
  }
  test("Q13. retry method with created config with sleep") {
    var counter = 0

    implicit val retryConfig: RetryConfig =
      RetryConfig(10, sleepMilliSeconds = 500)

    retry {
      println("hello from the code block!")
      counter += 1
    }

    counter shouldBe 10
  }
}
