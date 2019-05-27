object Q3 {
  /*
  Q3. Write the ~= operator for comparing doubles.
  The operator should return true iff 2 doubles are equal up to a small constant.
  The constant should be configurable but also have a default. Include tests.
   */

  implicit class ComparableDouble(doubleValue: Double) {
    def ~=(otherDoubleValue: Double, allowedDifference:Double = 0.00001): Boolean = Math.abs(doubleValue - otherDoubleValue) < allowedDifference
  }
}
