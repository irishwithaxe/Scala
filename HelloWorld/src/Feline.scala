trait Feline{
  def colour:String
  val sound:String = {
    this match {
      case Cat(_,_) => "meow"
      case Tiger(_) | Lion(_,_) | Panther (_) => "roar"
    }
  }
}

case class Cat(colour:String, food:String) extends  Feline
case class Tiger(colour:String) extends  Feline
case class Lion(colour:String, maneSize:Int) extends  Feline
case class Panther(colour:String) extends  Feline

object ChipShop{
  def willServe(cat:Cat): Boolean ={
    cat match {
      case Cat(_, "Chips") => true
      case _ => false
    }
  }
}

sealed trait Food
case object Antelope extends Food
case object TigerFood extends Food
case object Licorice extends Food
case class CatFood(food: String) extends Food

