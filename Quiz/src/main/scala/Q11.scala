object Q11 {
  /*
Q11. Write a function that gets an optional x,y and z and returns the first that is not None
   */

  def FirstNotNone[T](x: Option[T], y: Option[T], z: Option[T]): Option[T] = {
    if (x.isDefined) x
    else if (y.isDefined) y
    else z
  }
}
