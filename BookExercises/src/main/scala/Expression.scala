import java.util.function.IntToDoubleFunction

sealed trait Expression {
  def eval: Result[Double] = {
    this match {
      case Number(value) => Success(value)

      case Addition(left, right) =>
        left.eval match {
          case Failure(r) => Failure(r)
          case Success(lr) =>
            right.eval match {
              case Failure(r)  => Failure(r)
              case Success(rr) => Success(lr + rr)
            }
        }

      case Substraction(left, right) =>
        left.eval match {
          case Failure(r) => Failure(r)
          case Success(lr) =>
            right.eval match {
              case Failure(r)  => Failure(r)
              case Success(rr) => Success(lr - rr)
            }
        }

      case Division(left, right) =>
        left.eval match {
          case Failure(r) => Failure(r)
          case Success(lr) =>
            right.eval match {
              case Failure(r) => Failure(r)
              case Success(rr) =>
                if (rr == 0) Failure("Division by zero")
                else Success(lr / rr)
            }
        }

      case SquareRoot(value) =>
        value.eval match {
          case Failure(r) => Failure(r)
          case Success(v) =>
            if (v < 0) Failure("Square root of negative number")
            else Success(Math.sqrt(v))
        }
    }
  }
}

final case class Number(value: Double) extends Expression
final case class Addition(left: Expression, right: Expression)
    extends Expression
final case class Substraction(left: Expression, right: Expression)
    extends Expression
final case class Division(left: Expression, right: Expression)
    extends Expression
final case class SquareRoot(value: Expression) extends Expression
