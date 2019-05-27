import scala.annotation.tailrec

class Adder(amount: Int){
  def apply(in:Int) = amount + in
}

case class Counter(count:Int = 0){
  def inc:Counter = inc()

  def dec:Counter = dec()

  def inc(value:Int = 1):Counter = {
    Counter(this.count + value)
  }

  def dec(value:Int = 1):Counter = {
    Counter(this.count - value)
  }

  def adjust(adder: Adder): Counter = {
    Counter(adder(this.count))
  }
}

val eleven = Counter(14).dec.dec.dec.count

case class Person(firstName:String, lastName:String){
  def name = s"$firstName $lastName"
}

object Person{
  def apply(name:String): Person = {
    val parts = name.split(" ")
    Person(parts(0), parts(1))
  }
}

sealed trait DivisionResult {}
final case class Finite(result:Int) extends  DivisionResult{}
case object Infinite extends  DivisionResult{}

object divide {
  def apply(x:Int, y:Int): DivisionResult = {
    y match{
      case 0 => Infinite
      case _ => Finite(x / y)
    }
  }
}

divide(4,0)
divide(13,4)

sealed trait Calculation
final case class Success (result: Int) extends Calculation
final case class Failure (reason: String) extends  Calculation

case object Calculator{
  def + (calculation:Calculation, operand: Int): Calculation = {
    calculation match {
      case Success(v) => Success(v + operand)
      case Failure(m) => Failure(m)
    }
  }

  def - (calculation:Calculation, operand: Int): Calculation = {
    calculation match {
      case Success(v) => Success(v - operand)
      case Failure(message) => Failure(message)
    }
  }

  def / (calculation: Calculation, operand: Int): Calculation = {
    calculation match {
      case Success(result) => {
        operand match {
          case 0 => Failure("Division by zero")
          case _ => Success(result / operand)
        }
      }
      case Failure(reason) => Failure(reason)
    }
  }
}

assert(Calculator.+(Success(1), 1) == Success(2))
assert(Calculator.-(Success(3), 1) == Success(2))
assert(Calculator.+(Failure("mmm"), 1) == Failure("mmm"))
assert(Calculator.-(Failure("mmm"), 1) == Failure("mmm"))

assert(Calculator./(Success(4), 2) == Success(2))
assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))

sealed trait IntList
case object End extends IntList
final case class Pair (head: Int, tail:IntList) extends IntList

object ILHelper{
  def length (list: IntList) : Int = length(list, 0)
  @tailrec  def length (list: IntList, accumulator: Int): Int = {
    list match {
      case End => 0
      case Pair(head, tail) => length(tail, accumulator + head)
    }
  }
}

val example = Pair(1, Pair(2, Pair(3, End)))
assert(ILHelper.length(example) == 3)
assert(ILHelper.length(example.tail) == 2)
assert(ILHelper.length(End) == 0)
