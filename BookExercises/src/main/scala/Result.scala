sealed trait Result[T]

final case class Success[T](result: T) extends Result[T] {
  private def equalToDouble(d: Double): Boolean = {
    result match {
      case dRes: Double =>
        val diff = d - dRes
        if (diff < 0.000001) true else false

      case _ => result == d
    }
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case dObj: Double          => equalToDouble(dObj)
      case Success(dObj: Double) => equalToDouble(dObj)

      case iObj: Int          => equalToDouble(iObj.toDouble)
      case Success(iObj: Int) => equalToDouble(iObj.toDouble)

      case Success(value) => result == value

      case _ => false
    }
  }
}

final case class Failure[T](reason: String) extends Result[T]
