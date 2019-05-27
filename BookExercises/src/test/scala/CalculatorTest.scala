import org.scalatest.FunSuite

class CalculatorTest extends FunSuite {
  test("inc") {
    assert(Calculator.+(Success(1), 1) == Success(2))
    assert(Calculator.+(Failure("mmm"), 1) == Failure("mmm"))
  }
  test("dec") {
    assert(Calculator.-(Success(3), 1) == Success(2))
    assert(Calculator.-(Failure("mmm"), 1) == Failure("mmm"))
  }
  test("div") {
    assert(Calculator./(Success(4), 2) == Success(2))
    assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
    assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))
  }

  test("calculate") {
    assert(Calculator.calculate("as", "+", "2") === "Can't parse operand1")
    assert(Calculator.calculate("0", "+", "a2") === "Can't parse operand2")
    assert(Calculator.calculate("0", "/", "0") === "Can't divide by zero")
    assert(Calculator.calculate("0", "%", "0") === "Unknown operator")

    assert(Calculator.calculate("1", "+", "3") === "4")
    assert(Calculator.calculate("42", "+", "3") === "45")
    assert(Calculator.calculate("42", "/", "42") === "1")
    assert(Calculator.calculate("1", "*", "1") === "1")
    assert(Calculator.calculate("1", "-", "10") === "-9")
  }
}
