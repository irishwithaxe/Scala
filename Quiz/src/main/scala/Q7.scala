object Q7 {
  /*
Q7. Convert a list of strings to a list of all the characters in all the strings
   */

  object ListToCharsConverter {
    def apply(strings: Seq[String]):Seq[String] = {
      strings.flatMap(string => {
        string.split("").filter(!_.isEmpty)
      })
    }
  }
}
