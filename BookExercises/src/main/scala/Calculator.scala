case object Calculator {
  def +(calculation: Result[Int], operand: Int): Result[Int] = {
    calculation match {
      case Success(v) => Success(v + operand)
      case Failure(m) => Failure(m)
    }
  }

  def -(calculation: Result[Int], operand: Int): Result[Int] = {
    calculation match {
      case Success(v)       => Success(v - operand)
      case Failure(message) => Failure(message)
    }
  }

  def /(calculation: Result[Int], operand: Int): Result[Int] = {
    calculation match {
      case Success(result) =>
        operand match {
          case 0 => Failure("Division by zero")
          case _ => Success(result / operand)
        }

      case Failure(reason) => Failure(reason)
    }
  }

  def calculate(operand1: String,
                operator: String,
                operand2: String): String = {
    def getInt(str: String): Option[Int] = {
      try {
        Some(str.toInt)
      } catch {

        case _: Exception => None // print error
      }
    }

    val op1 = getInt(operand1)
    val op2 = getInt(operand2)

    (op1, op2, operator) match {
      case (None, _, _)      => "Can't parse operand1"
      case (_, None, _)      => "Can't parse operand2"
      case (_, Some(0), "/") => "Can't divide by zero"
      case (Some(o1), Some(o2), "+" | "-" | "*" | "/") =>
        operator match {
          case "+" => op1.flatMap(o1 => op2.map(o1 + _)).get.toString
          case "-" => op1.flatMap(o1 => op2.map(o1 - _)).get.toString
          case "*" => op1.flatMap(o1 => op2.map(o1 * _)).get.toString
          case "/" => op1.flatMap(o1 => op2.map(o1 / _)).get.toString
        }
      case _ => "Unknown operator"
    }
  }
}
