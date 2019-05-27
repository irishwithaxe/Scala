object Q9 {
  /*
Q9. Consider the following definitions representing trees of integers. These definitions can be seen as an alternative representation of IntSet:
```
 abstract class IntTree
 case object EmptyTree extends IntTree
 case class Node(elem: Int, left: IntTree, right: IntTree) extends IntTree
Complete the following implementations of function contains and insert for IntTree’s.

def contains(t: IntTree, v: Int): Boolean = t match { ...
      ...
}
def insert(t: IntTree, v: Int): IntTree = t match { ...
      ...
}
   */

  abstract class IntTree {
    def contains(t: IntTree, v: Int): Boolean = t match {
      case EmptyTree               => false
      case Node(e, _, _) if v == e => true
      case Node(e, _, r) if v > e  => contains(r, v)
      case Node(e, l, _)           => contains(l, v)
    }

    def insert(t: IntTree, v: Int): IntTree = t match {
      case EmptyTree               => Node(v, EmptyTree, EmptyTree)
      case Node(e, l, r) if v == e => Node(e, l, r)
      case Node(e, l, r) if v > e  => Node(e, l, insert(r, v))
      case Node(e, l, r)           => Node(e, insert(l, v), r)
    }

    def incl(v: Int): IntTree = this.insert(this, v)

    def contains(v: Int): Boolean = this.contains(this, v)
  }

  case object IntTree {
    def apply(value: Int): IntTree = Node(value, EmptyTree, EmptyTree)
  }

  case object EmptyTree extends IntTree
  case class Node(elem: Int, left: IntTree, right: IntTree) extends IntTree

}
