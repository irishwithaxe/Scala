object Q6 {
  /*
Q6. Given a string containing words separated by space, find:
- The longest word
- The most common word
- The most common letter
Create a map from letter to a set of words it appear in

   */

  final case class PhraseAnalyser(phrase: String) {
    def longestWord: String = {
      phrase
        .split(' ')
        .foldLeft("")((longest, word) =>
          if (longest.length > word.length) longest else word)
    }

    private def mostCommonValueInArray[T](array: Array[T]): T = {
      array
        .foldLeft(Map[T, Int]())((wordsMap, word) => {
          val exist = wordsMap get word
          exist match {
            case None        => wordsMap + (word -> 1)
            case Some(value) => wordsMap + (word -> (value + 1))
          }
        })
        .maxBy(item => item._2)
        ._1
    }

    def words: Array[String] = phrase.split(" ").filter(!_.isEmpty)

    def mostCommonWord: String =
      if (words.length == 0) "" else mostCommonValueInArray(words)

    def mostCommonLetter: String =
      if (words.length == 0) ""
      else
        mostCommonValueInArray(words.flatMap(_.split("")))

    def mapLetterToWords: Map[String, Seq[String]] = {
      words.distinct.foldLeft(Map[String, Seq[String]]())(
        (letterToWords, word) => {
          word
            .split("")
            .foldLeft(letterToWords)((map, letter) => {
              val exist = map get letter
              exist match {
                case None      => map + (letter -> Seq(word))
                case Some(seq) => map + (letter -> (seq :+ word).distinct)
              }
            })
        })
    }
  }
}
