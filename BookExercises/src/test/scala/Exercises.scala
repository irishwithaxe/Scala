import org.scalatest.{Assertion, FunSuite}

class Exercises extends FunSuite {
  test("list with flatMap") {
    assert(List(1, 2, 3).flatMap(x => List(x, -x)) == List(1, -1, 2, -2, 3, -3))
  }

  test("list with Option") {
    val list = List[Option[Int]](Some(1), Some(2), Some(3))
    val expected = List[Option[Int]](Some(1), None, Some(3))

    assert(list.flatMap({
      case None    => List(None)
      case Some(v) => if (v % 2 == 0) List(None) else List(Some(v))
    }) == expected)

    assert(list.map({
      case None    => None
      case Some(v) => if (v % 2 == 0) None else Some(v)
    }) == expected)
  }

  test("sum of two options") {
    object sumOptions {
      def sumFor(op1: Option[Int], op2: Option[Int]): Option[Int] = {
        for {
          iv1 <- op1
          iv2 <- op2
        } yield iv1 + iv2
      }

      def sumFor(op1: Option[Int],
                 op2: Option[Int],
                 op3: Option[Int]): Option[Int] = {
        for {
          iv1 <- op1
          iv2 <- op2
          iv3 <- op3
        } yield iv1 + iv2 + iv3
      }

      def sumFlat(op1: Option[Int], op2: Option[Int]): Option[Int] = {
        op1.flatMap(o1 => op2.map(o2 => o1 + o2))
      }

      def sumFlat(op1: Option[Int],
                  op2: Option[Int],
                  op3: Option[Int]): Option[Int] = {
        op1.flatMap(o1 => op2.flatMap(o2 => op3.map(o3 => o1 + o2 + o3)))
      }

      def test2(f: (Option[Int], Option[Int]) => Option[Int]): Unit = {
        assert(f(Some(1), Some(2)) === Some(3))
        assert(f(None, Some(2)) === None)
        assert(f(Some(2), None) === None)
        assert(f(None, None) === None)
      }

      def test3(
          f: (Option[Int], Option[Int], Option[Int]) => Option[Int]): Unit = {
        assert(f(Some(1), Some(2), Some(3)) === Some(6))
        assert(f(None, Some(2), None) === None)
        assert(f(Some(2), None, Some(52)) === None)
        assert(f(None, None, None) === None)
      }
    }

    sumOptions.test2(sumOptions.sumFor)
    sumOptions.test2(sumOptions.sumFlat)
    sumOptions.test3(sumOptions.sumFor)
    sumOptions.test3(sumOptions.sumFlat)
  }

  test("sorting int things") {
    implicit val a42lf4: Ordering[Int] =
      Ordering.fromLessThan(Math.abs(_) < Math.abs(_))

    assert(List(-4, -1, 0, 2, 3).sorted(a42lf4) == List(0, -1, 2, 3, -4))
    assert(List(-4, -3, -2, -1).sorted(a42lf4) == List(-1, -2, -3, -4))

    assert(List(-4, -1, 0, 2, 3).sorted == List(0, -1, 2, 3, -4))
    assert(List(-4, -3, -2, -1).sorted == List(-1, -2, -3, -4))
  }

  test("sorting rational things") {
    final case class Rational(numerator: Int, denominator: Int) {
      def toDouble: Double = numerator * 1.0 / denominator
    }

    implicit val a23L9: Ordering[Rational] =
      Ordering.fromLessThan(_.toDouble < _.toDouble)

    assert(
      List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted ==
        List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))
  }

  test("equality with type class") {
    trait Equal[A] {
      def equals(a1: A, a2: A): Boolean
    }

    final object Eq {
      def apply[A](o1: A, o2: A)(implicit equal: Equal[A]): Boolean =
        equal.equals(o1, o2)
    }

    final case class Person(name: String, email: String)
    final object Person {
      implicit val comparer: Equal[Person] = ComparePerson.Comparator
    }

    object ComparePersonByEmail {
      implicit object Comparator extends Equal[Person] {
        override def equals(p1: Person, p2: Person): Boolean =
          p1.email == p2.email
      }
    }
    object ComparePerson {
      implicit object Comparator extends Equal[Person] {
        override def equals(p1: Person, p2: Person): Boolean =
          p1.email == p2.email && p1.name == p2.name
      }
    }

    object example {
      def call: Assertion = {
        import ComparePersonByEmail._
        assert(
          Eq(Person("Ivan Petrov", "jb@gb.com"),
             Person(email = "jb@gb.com", name = "James Bond")))
      }
    }

    example.call
  }

  test("enrichment of int") {
    object IntImplicits {
      implicit class OhEah(data: Int) {
        def ohEah: Seq[String] = {
          if (data < 0) Seq[String]() else (1 to data).map(_ => "Oh yea!")
        }

        def times(f: Int => Unit): Unit = {
          (0 until data).foreach(f(_))
        }
      }
    }

    import IntImplicits._
    assert(7.ohEah.size == 7)
    assert(2.ohEah.head == "Oh yea!")
    assert(-2.ohEah.size == 0)

    // 6.times(n => println(s"this is number $n"))
    // 4.times(_ => println("Oh yea!"))
  }

  test("type enrichment + type class equal operation") {
    trait Equal[T] {
      def superEqual(obj1: T, obj2: T): Boolean
    }

    object StringEqualCaseInsensitive {
      implicit object Comparator extends Equal[String] {
        override def superEqual(obj1: String, obj2: String): Boolean =
          obj1.toLowerCase == obj2.toLowerCase
      }
    }

    object StringEqualLength {
      implicit object Comparator extends Equal[String] {
        override def superEqual(obj1: String, obj2: String): Boolean =
          obj1.length == obj2.length
      }
    }

    implicit class StringEqual(str: String) {
      def ===(str1: String)(implicit comparator: Equal[String]): Boolean =
        comparator.superEqual(str, str1)
    }

    object t1 {
      def call: Assertion = {
        import StringEqualCaseInsensitive._
        assert("abcd" === "AbCD")
        assert(!("abc1" === "abcd"))
      }
    }

    object t2 {
      def call: Assertion = {
        import StringEqualLength._
        assert("av34" === "asdg")
        assert(!("bgf" === "bgf "))
      }
    }

    t1.call
    t2.call
  }

  test("positive extractor") {
    object Positive {
      def unapply(arg: Int): Option[Int] = {
        if (arg > 0) Some { arg } else None
      }
    }

    implicit class IntIsPositive(value: Int) {
      def isPositive12: String = {
        value match {
          case Positive(_) => "Yes"
          case _           => "No"
        }
      }
    }
    assert(0.isPositive12 == "No")
    assert((-1).isPositive12 == "No")
    assert(2.isPositive12 == "Yes")
  }

  test("titlecase extractor") {
    object TitleCase {
      def unapply(arg: String): Option[String] = {
        val words = arg.split(" ").map { word =>
          word.head.toString.toUpperCase + word.tail.toLowerCase()
        }
        Some(words.mkString(" "))
      }
    }

    assert(
      "Sir Lord Doctor David Gurnell" ==
        ("sir lord doctor david gurnell" match {
          case TitleCase(str) => str
        })
    )
  }
}
