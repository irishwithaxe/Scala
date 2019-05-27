import org.scalatest.FunSuite

class BinTreeTest extends FunSuite {
  test("sum via fold") {
    val example =
      Node[Int](Node(Node(Leaf(2), Leaf(3)), Leaf(2)), Node(Leaf(1), Leaf(1)))

    val sum = (tree: BinTree[Int]) => {
      tree.fold[Int]((left, right) => left + right)(l => l): Int
    }

    assert(sum(example) == 9)
    assert(sum(example.left) == 7)
    assert(sum(example.right) == 2)
    assert(sum(Node(Leaf(-2), Leaf(2))) == 0)
  }

  test("double via fold") {
    val example = Node(Node(Leaf(2), Leaf(1)), Node(Leaf(3), Leaf(1)))
    val expected = Node(Node(Leaf(4), Leaf(2)), Node(Leaf(6), Leaf(2)))

    assert(example.fold[BinTree[Int]]((left,right) => Node(left,right)) (value => Leaf(value * 2)) == expected)
  }

}
