import scala.annotation.tailrec

sealed trait LinkedList[A] {
  def length: Int = fold[Int](0, (_, tail) => 1 + tail)

  @tailrec final def contains(value: A): Boolean = {
    this match {
      case End()       => false
      case Pair(h, tl) => if (value == h) true else tl.contains(value)
    }
  }

  @tailrec final def apply(index: Int): Result[A] = {
    this match {
      case End()       => Failure("Index out of range.")
      case Pair(h, tl) => if (index == 0) Success(h) else tl.apply(index - 1)
    }
  }

  def fold[B](end: B, f: (A, B) => B): B = {
    this match {
      case End()       => end
      case Pair(h, tl) => f(h, tl.fold(end, f))
    }
  }

  def map[B](f: A => B): LinkedList[B] = {
    this match {
      case End()       => End[B]()
      case Pair(h, tl) => Pair(f(h), tl.map(f))
    }
  }
}

final case class End[A]() extends LinkedList[A]
final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

/*
  def product: Int = product(this)
  @tailrec private def product(list: LinkedList, accumulator: Int = 1): Int = {
    list match {
      case End()              => accumulator
      case Pair(head, tail) => product(tail, accumulator * head)
    }
  }

  def double : LinkedList = {
    this match {
      case End => End
      case Pair(head, tail) => Pair(head * 2, tail.double)
    }
  }
 */
