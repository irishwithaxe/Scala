sealed trait BinTree[A] {
  def fold[B](node: (B, B) => B)(leaf: A => B): B = {
    this match {
      case Leaf(value) => leaf(value)
      case Node(l, r)  => node(l.fold(node)(leaf), r.fold(node)(leaf))
    }
  }

  /*
  def sum: Int = {
    this match {
      case Leaf(value) => value
      case Node(l, r)  => l.sum + r.sum
    }
  }

  def double: BinTree = {
    this match {
      case Leaf(value) => Leaf(value * 2)
      case Node(l, r)  => Node(l.double, r.double)
    }
  }

 */
}
final case class Node[A](left: BinTree[A], right: BinTree[A]) extends BinTree[A]
final case class Leaf[A](value: A) extends BinTree[A]
