package Geometry

sealed trait Shape {
  def sides: Int

  def perimeter: Double

  def area: Double

  def color: Color
}

final case class Circle(radius: Double, color: Color) extends Shape {
  val sides: Int = 1
  val perimeter: Double = math.Pi * radius * 2
  val area: Double = math.Pi * radius * radius
}

sealed trait Rectangular extends Shape {
  def height: Double

  def width: Double

  val sides: Int = 4
  val perimeter: Double = height * 2 + width * 2
  val area: Double = height * width
}

final case class Rectangle(height: Double, width: Double, color: Color) extends Rectangular

final case class Square(side: Double, color: Color) extends Rectangular {
  val height: Double = side
  val width: Double = side
}