import org.scalatest.{FunSuite, Matchers}
import Q9._

class Q9Test extends FunSuite with Matchers {
  test("contains and insert for IntTree") {
    val defaultSet1 = IntTree(10)
      .incl(25)
      .incl(3)
      .incl(20)
      .incl(4)
      .incl(7)
      .incl(15)
      .incl(1)
      .incl(2)
      .incl(11)

    val defaultSet2 = IntTree(15)
      .incl(1)
      .incl(20)
      .incl(2)
      .incl(11)
      .incl(10)
      .incl(4)
      .incl(7)
      .incl(5)
      .incl(25)
      .incl(3)

    val defaultSet3 = IntTree(1)
      .incl(2)
      .incl(3)
      .incl(4)
      .incl(5)
      .incl(7)
      .incl(10)
      .incl(11)
      .incl(15)
      .incl(20)
      .incl(25)

    defaultSet1.contains(10) shouldBe true
    defaultSet1.contains(15) shouldBe true
    defaultSet1.contains(6) shouldBe false
    defaultSet1.contains(9) shouldBe false

    defaultSet2.contains(10) shouldBe true
    defaultSet2.contains(15) shouldBe true
    defaultSet2.contains(6) shouldBe false
    defaultSet2.contains(9) shouldBe false

    defaultSet3.contains(10) shouldBe true
    defaultSet3.contains(15) shouldBe true
    defaultSet3.contains(6) shouldBe false
    defaultSet3.contains(9) shouldBe false
  }
}
