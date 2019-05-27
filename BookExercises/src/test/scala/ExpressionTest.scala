import org.scalatest.FunSuite

class ExpressionTest extends FunSuite {
  test("number eval") {
    assert(Number(5.543).eval === 5.543)
  }

  test("addition eval") {
    assert(Addition(Number(3.0), Number(2)).eval === 5.0)
    assert(Addition(Addition(Number(-1), Number(-3)), Number(2)).eval === -2.0)
  }

  test("subtraction eval") {
    assert(Substraction(Number(4.4), Number(3)).eval === Success(1.4))
    assert(
      Substraction(Number(-5.9), Substraction(Number(4.4), Number(-9.7))).eval === -20.0)
  }

  test("division eval") {
    assert(Division(Number(4), Number(2)).eval === 2)
  }

  test("squareRoot eval") {
    assert(SquareRoot(Number(9)).eval === 3.0)
  }

  test("from book") {
    assert(
      Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval == Failure(
        "Square root of negative number"))
    assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval === 4)
    assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))
  }
}
