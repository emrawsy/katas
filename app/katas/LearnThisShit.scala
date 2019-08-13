package katas

import scala.language.postfixOps

object LearnThisShit {

  //accumulator that adds a repeat of the letter index, first letter capitalised eg. "abcd" -> "a-Bb-Ccc-Dddd"
  def accum(s: String) = {
    s.indices.map(i => s(i).toLower.toString * (i + 1) capitalize).mkString("-")
  }

  //checks that a square is a square returns true if its a square
  def isSquare(n: Int): Boolean = {
    math.sqrt(n).isWhole
  }

  //The program reports the nth day (as an integer) on which the evaporator will be out of use.
  def evaporator(content: Double, evapPerDay: Int, threshold: Int): Int = {
    (math.log(threshold / 100.0) / math.log(1 - evapPerDay / 100.0)).toInt + 1
  }


  def findMissingLetter(chars: Array[Char]): Char = {
    val alphabet = ('a' to 'z') ++ ('A' to 'Z')

    val substr = alphabet.splitAt(alphabet.indexOf(chars.head))
    substr._2.zip(chars).find {
      case (correct, option) =>
        correct != option
    }.get._1
  }

  def countChars(string: String): Map[Char, Int] = {
    string.foldLeft[Map[Char, Int]](Map.empty)((char, num) =>
      char + (num -> (char.getOrElse(num, 0) + 1)))
  }

  //  def duplicateCount(str: String): Int = {
  //    countChars(str.toLowerCase).values.map(_ -1).count(_ > 0)
  //  }

  def duplicateCount(str: String): Int = {
    str.toSeq.groupBy(_.toLower).count(_._2.size > 1)
  }

  def mxdiflg(a1: List[String], a2: List[String]): Int = {

    if (a1.isEmpty || a2.isEmpty) {
      -1
    } else {
      (a1 ++ a2).size
    }
  }
}