import org.scalatest.{FunSuite, Matchers}
import Q14._

class Q14Test extends FunSuite with Matchers {
  test("Q14 int vector calculator") {
    val calc = new StaticVectorCalculator(Vector(1, 2, 3, 4, 5, 6, 7))
    println("y is " + calc.y)
    println("x is " + calc.x)
    println("z is " + calc.z)

    println("x is " + calc.x)
    println("y is " + calc.y)
    println("z is " + calc.z)

    println("x is " + calc.x)
    println("y is " + calc.y)
    println("z is " + calc.z)
  }

  test("Q14 double vector calculator") {
    val calc = new StaticVectorCalculator(Vector(10d, 2f, 3, 4d, 5, 6, 7))
    println("y is " + calc.y)
    println("x is " + calc.x)
    println("z is " + calc.z)

    println("x is " + calc.x)
    println("y is " + calc.y)
    println("z is " + calc.z)

    println("x is " + calc.x)
    println("y is " + calc.y)
    println("z is " + calc.z)
  }
}
