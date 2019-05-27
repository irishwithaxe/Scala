package Geometry

object Draw {
  def apply(shape: Shape): Unit = {
    val color = Color.toStr(shape.color)
    shape match {
      case Circle(r: Double, _) => println(s"Circle with radius $r, $color")
      case Rectangle(h: Double, w: Double, _) => println(s"Rectangle with h:$h and w:$w, $color")
      case Square(s: Double, _) => println(s"Square with side $s, $color")
    }
  }
}
