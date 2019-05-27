object Q14 {
  /*
Q14. Design a class that is given a vector of numbers in the constructor and exposes:
x: a vector with the square of all elements in the input vector
y: the sum of x
z: the square root of y
nothing should be calculated in the constructor of the class assume the calculation of x,y,z can take a lot of time, and should only be done once (at most)

   */
  trait VectorCalculatorOperations[T] {
    def square(v1: T): T
    def sum(v1: T, v2: T): T
    def squareRoot(v1: T): T
    def defaultSumValue: T
  }

  implicit object intOps extends VectorCalculatorOperations[Int] {
    override def square(v1: Int): Int = v1 * v1
    override def sum(v1: Int, v2: Int): Int = v1 + v2
    override def squareRoot(v1: Int): Int = Math.sqrt(v1).toInt
    override def defaultSumValue: Int = 0
  }

  implicit object doubleOps extends VectorCalculatorOperations[Double] {
    override def square(v1: Double): Double = v1 * v1
    override def sum(v1: Double, v2: Double): Double = v1 + v2
    override def squareRoot(v1: Double): Double = Math.sqrt(v1)
    override def defaultSumValue: Double = 0.0
  }

  class StaticVectorCalculator[A](vector: Vector[A])(
      implicit calcOps: VectorCalculatorOperations[A]) {
    lazy val x: Vector[A] = {
      println("calc x")
      vector.map(entry => calcOps.square(entry))
    }

    lazy val y: A = {
      println("calc y")
      x.foldLeft(calcOps.defaultSumValue)((sum, entry) =>
        calcOps.sum(sum, entry))
    }

    lazy val z: A = {
      println("calc z")
      calcOps.squareRoot(y)
    }
  }
}
