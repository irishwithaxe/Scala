package Geometry

sealed trait Color {
  def R: Short

  def G: Short

  def B: Short

  def isLight: Boolean = {
    if (R < 100 && G < 100 && B < 100) true else false
  }
}

object Color {
  def apply(r: Short, g: Short, b: Short): Color = {
    (r, g, b) match {
      case (255, 255, 0) => Yellow()
      case (255, 0, 0) => Red()
      case (255, 0, 255) => Pink()
      case _ => CustomColor(r, g, b)
    }
  }

  def toStr(color: Color): String = {
    color match {
      case Yellow() => "Yellow"
      case Red() => "Red"
      case Pink() => "Pink"
      case CustomColor(_, _, _) => if (color.isLight) "Light color" else "Dark color"
    }
  }
}

final case class Pink() extends Color {
  val R: Short = 255
  val G: Short = 0
  val B: Short = 255
}

final case class Yellow() extends Color {
  val R: Short = 255
  val G: Short = 255
  val B: Short = 0
}

final case class Red() extends Color {
  val R: Short = 255
  val G: Short = 0
  val B: Short = 0
}

final case class CustomColor(R: Short, G: Short, B: Short) extends Color {}
