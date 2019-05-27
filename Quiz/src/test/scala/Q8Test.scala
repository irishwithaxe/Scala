import org.scalatest.{FunSuite, Matchers}
import Q8._

class Q8Test extends FunSuite with Matchers {
  val defaultSet1: IntSet = new EmptySet()
    .incl(10)
    .incl(5)
    .incl(25)
    .incl(3)
    .incl(20)
    .incl(4)
    .incl(7)
    .incl(15)
    .incl(1)
    .incl(2)
    .incl(11)

  val defaultSet2: IntSet = new EmptySet()
    .incl(15)
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

  val defaultSet3: IntSet = new EmptySet()
    .incl(1)
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

  def toSortedVector(set: IntSet): Vector[Int] =
    set.foldLeft(Vector[Int]())((set, elem) => set :+ elem)

  test("foldLeft for set") {
    val sortedVector = Vector(1, 2, 3, 4, 5, 7, 10, 11, 15, 20, 25)

    toSortedVector(defaultSet1) shouldEqual sortedVector
    toSortedVector(defaultSet2) shouldEqual sortedVector
    toSortedVector(defaultSet3) shouldEqual sortedVector
  }

  test("contains and incl for set") {
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

  test("Q8. union test") {
    val set1 = new EmptySet().incl(7).incl(3).incl(10).incl(2).incl(1)
    val set2 = new EmptySet().incl(8).incl(3).incl(11).incl(4).incl(1)

    val vector = toSortedVector(set1.union(set2))

    vector shouldEqual Vector(1, 2, 3, 4, 7, 8, 10, 11)
  }

  test("Q8. intersection test") {
    val set1 = new EmptySet().incl(7).incl(3).incl(10).incl(2).incl(1)
    val set2 = new EmptySet().incl(8).incl(3).incl(11).incl(4).incl(1)

    val vector = toSortedVector(set1.intersection(set2))

    vector shouldEqual Vector(1, 3)
  }

  test("Q8. exclude test") {
    val sortedVector1 = Vector(1, 2, 3, 4, 5, 7, 11, 20, 25)
    toSortedVector(defaultSet1.excl(10).excl(15).excl(6).excl(9)) shouldEqual sortedVector1
    toSortedVector(defaultSet2.excl(10).excl(15).excl(6).excl(9)) shouldEqual sortedVector1
    toSortedVector(defaultSet3.excl(10).excl(15).excl(6).excl(9)) shouldEqual sortedVector1

    val sortedVector2 = Vector(2, 3, 4, 7, 10, 15, 20)
    toSortedVector(defaultSet1.excl(1).excl(5).excl(11).excl(25)) shouldEqual sortedVector2
    toSortedVector(defaultSet2.excl(1).excl(5).excl(11).excl(25)) shouldEqual sortedVector2
    toSortedVector(defaultSet3.excl(1).excl(5).excl(11).excl(25)) shouldEqual sortedVector2
  }
}
