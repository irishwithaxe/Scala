import org.scalatest.FunSuite

class LinkedListTest extends FunSuite {
  test("length") {
    val example = Pair(1, Pair(2, Pair(3, End())))

    assert(example.length == 3)
    assert(example.tail.length == 2)
    assert(End().length == 0)
  }

  test("contains int") {
    val example = Pair(1, Pair(2, Pair(3, End())))

    assert(example.contains(3))
    assert(!example.contains(4))
    assert(!End().contains(0))
  }

  test("contains string") {
    var example = Pair("testtext1", Pair("testtext2", Pair("texttext3", End())))

    assert(example.contains("texttext3"))
    assert(!example.contains("not an Int"))
  }

  test("apply") {
    val example = Pair(1, Pair(2, Pair(3, End())))

    assert(example(0) === 1)
    assert(example(1) === 2)
    assert(example(2) === 3)

    assert(example(3) === Failure("Index out of range."))
  }

  test("map"){
    val example = Pair(1, Pair(2, Pair(3, End())))

    assert(example.map(_ * 2) == Pair(2,Pair(4, Pair(6, End()))))
    assert(example.map(_ + 1) == Pair(2,Pair(3, Pair(4, End()))))
    assert(example.map(_ / 3) == Pair(0,Pair(0, Pair(1, End()))))
  }

  /*
  test("product") {
    val example = Pair(1, Pair(2, Pair(3, End())))

    assert(example.product == 6)
    assert(example.tail.product == 6)
    assert(End.product == 1)
  }

  test("double") {
    val example = Pair(1, Pair(2, Pair(3, End())))

    assert(example.double == Pair(2, Pair(4, Pair(6, End))))
    assert(example.tail.double == Pair(4, Pair(6, End)))
    assert(End.double == End)
  }
 */

}
