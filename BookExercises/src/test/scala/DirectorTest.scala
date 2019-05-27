import org.scalatest.FunSuite;

class DirectorTest extends FunSuite {
  test("cris nolan films") {
    val vals = for (film <- FilmsAndDirectors.nolan.films) yield film.name
    assert(vals == Seq("Memento", "Dark Knight", "Inception"))
  }

  test("all films") {
    val allfilms = for {
      director <- FilmsAndDirectors.directors
      film <- director.films
    } yield film.name

    assert(allfilms.length == 12)
    assert(allfilms.contains("Memento"))
    assert(allfilms.contains("The Hunt for Red October"))
  }

  test("films ordered by IMDB") {
    val films = (for {
      director <- FilmsAndDirectors.directors
      film <- director.films
    } yield film).sortWith(_.imdbRating > _.imdbRating)

    assert(films.head.name == "Dark Knight")
    assert(films.tail.head.imdbRating == 8.8)
  }

  test ("printing films with director"){
    var printed = for{
      director <- FilmsAndDirectors.directors
      film <- director.films
    } yield s"Tonight only! ${film.name} by ${director.firstName} ${director.lastName}!"

    assert(printed.length == 12)
    assert(printed.contains("Tonight only! Gran Torino by Clint Eastwood!"))
  }
}
