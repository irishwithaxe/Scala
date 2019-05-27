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





