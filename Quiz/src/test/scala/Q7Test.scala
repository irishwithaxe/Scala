import org.scalatest.{FunSuite, Matchers}
import Q7._

class Q7Test extends FunSuite with Matchers {
  val chars = ListToCharsConverter(Seq("word", "another", "tree", "rock"))
  chars should equal(Seq("w", "o", "r", "d", "a", "n", "o", "t", "h", "e", "r", "t", "r", "e", "e", "r", "o", "c", "k"))
}
