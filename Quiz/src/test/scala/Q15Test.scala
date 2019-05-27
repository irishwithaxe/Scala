import org.scalatest.{FunSuite, Matchers}
import Q15._

class Q15Test extends FunSuite with Matchers {
  test("Q15. median for Seq[Int]") {
    Seq(1, 2, 3, 4, 5).median shouldBe Some(3)
    Seq(1, 12, 3, 1, 50).median shouldBe Some(3)
    Seq(-200, 12, 3, 0, 50000).median shouldBe Some(3)
    Seq[Int]().median shouldBe None
  }

  test("Q15. median for Seq[Double]") {
    implicit object DoubleSeqOp extends SequenceOperations[Double] {
      override def sortFunc(v1: Double, v2: Double): Boolean = v1 < v2
      override def median(v1: Double, v2: Double): Double = (v1 + v2) / 2
    }

    Seq(1d, 2, 3, 4, 5.53).median shouldBe Some(3d)
    Seq(1, 2, 3.14, 4, 5).median shouldBe Some(3.14)
  }
}
