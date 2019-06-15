val g: PartialFunction[List[Int],String] = {
  case Nil => "one"
  case x :: rest =>
    rest match {
      case Nil => "two"
    }
}

g.isDefinedAt(List(1,2,3,4))
g.isDefinedAt(List(1,2,3))
g.isDefinedAt(List(1,2))
g.isDefinedAt(List(1))
g.isDefinedAt(List.empty[Int])
