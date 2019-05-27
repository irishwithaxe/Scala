sealed trait BinTreeP {
  def sum: Int
  def double: BinTreeP
}
final case class NodeP(left: BinTreeP, right: BinTreeP) extends BinTreeP {
  override def sum: Int = {
    left.sum + right.sum
  }

  override def double: BinTreeP = {
    NodeP(left.double, right.double)
  }
}

final case class LeafP(value: Int) extends BinTreeP {
  override def sum: Int = value

  override def double: BinTreeP = LeafP(value * 2)
}
