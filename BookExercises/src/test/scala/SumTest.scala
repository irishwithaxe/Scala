import org.scalatest.FunSuite

class SumTest extends FunSuite {
  test("map") {
    assert(SumL[String, Int]("text").map(x => x * 0.22) == SumL("text"))
    assert(SumR[String, Int](10).map(x => x * 0.22) == SumR(2.2))
  }

  test("fold") {
    assert(SumL[Int, Double](25).fold(fl => fl * 0.1, fr => fr) == 2.5)
    assert(SumR[Int, Double](2.5).fold(fl => fl, fr => fr * 10) == 25)
  }
}
