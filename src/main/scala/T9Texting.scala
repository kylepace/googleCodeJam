
/*Problem

The Latin alphabet contains 26 characters and telephones only have ten digits on the keypad.
We would like to make it easier to write a message to your friend using a sequence of keypresses to indicate
the desired characters. The letters are mapped onto the digits as shown below. To insert the character B for
instance, the program would press 22. In order to insert two characters in sequence from the same key, the
user must pause before pressing the key a second time. The space character ' ' should be printed to indicate
a pause. For example, 2 2 indicates AA whereas 22 indicates B.

Input

The first line of input gives the number of cases, N. N test cases follow. Each case is a line of text formatted as

desired_message

Each message will consist of only lowercase characters a-z and space characters ' '. Pressing zero emits a space.

Output

For each test case, output one line containing "Case #x: " followed by the message translated into the sequence of keypresses.

Sample

Input

Output
4
hi
yes
foo  bar
hello world

Case #1: 44 444
Case #2: 999337777
Case #3: 333666 6660 022 2777
Case #4: 4433555 555666096667775553

*/

object T9Texting {
  private def generateMap() : Map[String, String] = {
    val codes = "abcdefghijklmnotuvpqrswxyz".zipWithIndex.zip("22233344455566688877779999")

    def generateCode(index: Int, number: String): String = {
      if (index >= 18)
        number * (((index - 18) % 4) + 1)
      else
        number * ((index % 3) + 1)
    }

    codes.map(c => {
      generateCode(c._1._2, c._2.toString) -> c._1._1.toString
    }).toMap ++ Map("0" -> " ")
  }

  lazy val codeMap = generateMap()

  def parseCodes(code: String) = {
    def parseCodes_h(previousChar: Option[Char], agg: String, codes: List[Char], parsed: List[String]) : List[String] = {
      if (codes.isEmpty) {
        return parsed ++ List(agg)
      }
      previousChar match {
        case None => parseCodes_h(Some(codes.head), codes.head.toString, codes.tail, parsed)
        case Some(c) =>
          if (c == codes.head)
            parseCodes_h(Some(codes.head), agg + codes.head, codes.tail, parsed)
          else if (codes.head == ' ')
            parseCodes_h(None, "", codes.tail, parsed ++ List(agg))
          else
            parseCodes_h(Some(codes.head), codes.head.toString, codes.tail, parsed ++ List(agg))
      }
    }

    parseCodes_h(None, "", code.toList, List())
  }

  def convert(code: String) = {
    for {
      c <- parseCodes(code)
      p <- codeMap getOrElse(c, "")
    } yield p
  }.mkString
}
