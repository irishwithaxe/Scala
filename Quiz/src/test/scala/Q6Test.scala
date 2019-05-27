import org.scalatest.{FunSuite, Matchers}
import Q6._

class Q6Test extends FunSuite with Matchers {
  test("split understanding") {
    val array = "some words".split(" ").flatMap(_.split(""))
    array should equal(Array("s", "o", "m", "e", "w", "o", "r", "d", "s"))
  }

  test("Q6. longest word") {
    var pa = PhraseAnalyser("")
    assert(pa.longestWord == "")

    pa = PhraseAnalyser(
      "a small group of words standing together as a conceptual unit typically forming a component of a clause")
    assert(pa.longestWord == "conceptual")
  }

  test("Q6. most common word") {
    var pa = PhraseAnalyser("")
    assert(pa.mostCommonWord == "")

    pa = PhraseAnalyser(
      "a small group of words standing together as a conceptual unit typically forming a component of a clause")
    assert(pa.mostCommonWord == "a")
  }

  test("Q6. most common letter") {
    var pa = PhraseAnalyser("")
    assert(pa.mostCommonLetter == "")

    pa = PhraseAnalyser("la small group")
    assert(pa.mostCommonLetter == "l")

    pa = PhraseAnalyser(
      "a small group of words standing together as a conceptual unit typically forming a component of a clause")
    assert(pa.mostCommonLetter == "a")
  }

  test("words"){
    var pa = PhraseAnalyser("")
    pa.words should equal (Array[String]())
  }

  test("Q6. map letters to words") {
    var pa = PhraseAnalyser("")
    pa.mapLetterToWords should equal(Map[String, Seq[String]]())

    pa = PhraseAnalyser("la small group")
    pa.mapLetterToWords should equal(
      Map[String, Seq[String]](
        "l" -> Seq("la", "small"),
        "a" -> Seq("la", "small"),
        "s" -> Seq("small"),
        "m" -> Seq("small"),
        "g" -> Seq("group"),
        "r" -> Seq("group"),
        "o" -> Seq("group"),
        "u" -> Seq("group"),
        "p" -> Seq("group")
      ))

    pa = PhraseAnalyser(
      "a small group of words standing together as a conceptual unit typically forming a component of a clause")
    assert(pa.mostCommonLetter == "a")
  }
}
