import org.scalatest.FunSuite

class BinTreePTest extends FunSuite {
  test("sum") {
    val example =
      NodeP(NodeP(NodeP(LeafP(2), LeafP(3)), LeafP(2)), NodeP(LeafP(1), LeafP(1)))

    assert(example.sum == 9)
    assert(example.left.sum == 7)
    assert(example.right.sum == 2)
    assert(NodeP(LeafP(-2), LeafP(2)).sum == 0)
  }

  test("double") {
    val example = NodeP(NodeP(LeafP(2),LeafP(1)),NodeP(LeafP(3), LeafP(1)))
    val expected = NodeP(NodeP(LeafP(4),LeafP(2)),NodeP(LeafP(6), LeafP(2)))

    assert(example.double == expected)
  }
}
