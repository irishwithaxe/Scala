sealed trait Sum[A, B] {
  def map[C](f: B => C): Sum[A, C] = {
    this match {
      case SumL(v) => SumL(v)
      case SumR(v) => SumR(f(v))
    }
  }

  def fold[C](fl: A => C, fr: B => C): C = {
    this match {
      case SumL(lv) => fl(lv)
      case SumR(rv) => fr(rv)
    }
  }

  def flatMap[C](f: B => Sum[A, C]): Sum[A, C] = {
    this match {
      case SumL(lv) => SumL(lv)
      case SumR(rv) => f(rv)
    }
  }
}

final case class SumL[A, B](value: A) extends Sum[A, B]
final case class SumR[A, B](value: B) extends Sum[A, B]
