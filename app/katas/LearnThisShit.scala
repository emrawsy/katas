package katas

import scala.annotation.tailrec
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

  def meeting(s: String): String = {

    val separateNames: Seq[(String, String)] =
      s.split(";").map(_.toUpperCase).map {
        firstAndLastName => {
          val splitName = firstAndLastName.split(":")
          (splitName.last, splitName.head)
          }.copy()
      }
    separateNames.sorted.mkString
  }

  //numbers to roman numerals
  def encodeRoman(decimal: Int): String = {
    val numerals = List(
      List("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"),
      List("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"),
      List("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
    )
    "M" * (decimal / 1000) + numerals(0)(decimal % 1000 / 100) + numerals(1)(decimal % 100 / 10) + numerals(2)(decimal % 10)
  }

  //roman numerals to numbers
  def decode(string: String): Int = {
    val dictionary = List("I" -> 1, "IV" -> 4, "V" -> 5, "IX" -> 9, "X" -> 10, "XL" -> 40, "L" -> 50, "XC" -> 90, "C" -> 100, "CD" -> 400, "D" -> 500, "CM" -> 900, "M" -> 1000).reverse

    @tailrec
    def decode(value: Int, remaining: String): Int = {
      dictionary.find { case (k, _) => remaining.startsWith(k) } match {
        case Some((numeral, v)) =>
          val (_, rest) = remaining.splitAt(numeral.length)
          decode(value + v, rest)
        case None =>
          value
      }
    }

    decode(0, string)
  }

  //better idea
  def alphabetPlace(text: String): String = {
    text.filter(_.isLetter).map(_.toLower - 96).mkString(" ")
  }

  //my attempt
  def alphabetPosition(text: String): String = {
    def convert(char: Char): Int = {
      val alphabetConvert = ('a' to 'z').zip(1 to 26).toMap
      alphabetConvert(char)
    }

    val validString = text.toLowerCase.replaceAll("[^a-z]", "")
    validString.toList.map(x => convert(x)).mkString(" ")
  }


  def reverseWords(str: String): String = {
    str.split(" ", str.length).reverse.mkString(" ")
  }

  def toCamelCase(str: String): String = {
    val split = str.split("[-_]").map(_.toLowerCase)
    val tail = split.tail.map(x => x.head.toUpper + x.tail)
    split.head + tail.mkString
  }

  def solution(s: String): List[String] = {
    if (s.length % 2 == 1) {
      val newStr = s.concat("_")
      newStr.sliding(2, 2).toList
    } else {
      s.sliding(2, 2).toList
    }
  }

  //better solution using grouped and padTo
  def solutionTwo(s: String): List[String] = {
    s.padTo(s.size + s.size % 2, '_').grouped(2).toList
  }

  def middle(s: String): String = {
    if (s.length % 2 == 0) {
      val substr = s.splitAt(s.length / 2)
      val str1 = substr._1.last.toString
      val str2 = substr._2.head.toString
      str1.concat(str2)
    } else {
      s.splitAt(s.length / 2 + 1)._1.last.toString
    }
  }

  def countingSheep(num: Int): String = {
    (1 to num).map(i => s"$i sheep...").mkString
  }

  //or
  def countingSheep2(num: Int): String = {
    val numbers = (1 to num).toList.mkString(" sheep...")
    if (num > 0) {
      numbers + " sheep..."
    } else {
      ""
    }
  }

  def reverse(str: String) = {
    val list = str.split(" ").toList
    list.zipWithIndex.map(
      x => if (x._2 % 2 == 1) {
        x._1.reverse.mkString
      } else {
        x._1
      }
    ).mkString(" ")
  }

  def findShort(str: String) = {
    val list = str.toLowerCase.split(" ").toList
    list.sortWith(_.length < _.length).head.size
  }

  def fizzBuzzClock(time: String): String = {
    time.split(":").map(_.toInt) match {
      case Array(hour, 0) => Array.fill(if (hour % 12 == 0) 12 else hour % 12)("Cuckoo").mkString(" ")
      case Array(_, 30) => "Cuckoo"
      case Array(_, 15 | 45) => "Fizz Buzz"
      case Array(_, minute) if minute % 5 == 0 => "Buzz"
      case Array(_, minute) if minute % 3 == 0 => "Fizz"
      case _ => "tick"
    }
  }

  def fizzBuzzCuckooClock(time: String) = {
    val split = time.split(":").toList
    val minutes = split.last.toInt
    val hours = split.head.toInt

    if (hours == 0 && minutes == 0) {
      "Cuckoo " * 11 + "Cuckoo"
    }
    else if (minutes != 0 && minutes != 30 && minutes % 3 == 0 && minutes % 5 == 0) {
      "Fizz Buzz"
    }
    else if (minutes != 0 && minutes != 30 && minutes % 3 == 0) {
      "Fizz"
    }
    else if (minutes != 0 && minutes != 30 && minutes % 5 == 0) {
      "Buzz"
    }
    else if (minutes == 30) {
      "Cuckoo"
    }
    else if (minutes == 0 && hours > 12) {
      ("Cuckoo " * ((hours - 12) - 1)) + "Cuckoo"
    }
    else if (minutes == 0 && hours <= 12) {
      "Cuckoo " * (hours - 1) + "Cuckoo"
    }
    else "tick"
  }

  def concatOption(first: Option[String], second: Option[String]) = {
    (first, second) match {
      case (Some(first), Some(second)) => Option(first + second)
      case _ => None
    }
  }

  def getVowelCount(str: String): Int = {
    str.toLowerCase.replaceAll("[^aeiou]+", "").length
  }

  //takes any word in string over 5 letters and reverses them
  def spinWords(sentence: String) = {
    val list = sentence.split(" ").toList
    val zipped = list.map(_.length).zip(list)
    val newString = zipped map {
      case (i, str) if i >= 5 => str.reverse
      case (i, str) if i < 5 => str
    }
    newString.mkString(" ")
  }

  //nicer version of splitwords
  def spinWordAgain(sentence: String) = {
    sentence
      .split(" ")
      .map(x => if (x.size >= 5) x.reverse else x)
      .mkString(" ")
  }

  //ordered count of a string
  def orderedCount(chars: String): List[(Char, Int)] = {
    chars.distinct.map(c => (c, chars.count(_ == c))).toList
  }

  //sort string by last letter of each string
  def last(s: String): Array[String] = {
    s.split(' ').sortBy(_.last)
  }

  //backspace #
  def cleanString(s: String) = {
    s.foldLeft("") {
      case (acc, '#') => acc.dropRight(1)
      case (acc, c) => acc + c
    }
  }

  def positiveSum(arr: Array[Int]): Int = {
    arr.filter(_ > 0).sum
  }

  def summation(n: Int): Int = {
    (0 to n).sum
  }

  def crossover(chrom1: String, chrom2: String, index: Int): List[(String, String)] = {
    val indicies = index until chrom1.length
    indicies.toList.map(x => crossoverAtIndex(chrom1, chrom2, x))
  }

  def crossoverAtIndex(chrom1: String, chrom2: String, index: Int): (String, String) = {
    val first = chrom1.substring(0, index) + chrom2.substring(index)
    val second = chrom2.substring(0, index) + chrom1.substring(index)
    (first, second)
  }

  def movie(card: Int, ticket: Int, perc: Double) = {
    import scala.annotation.tailrec
    @tailrec
    def countVisits(n: Int, priceA: Int, priceB: Double, memberTicket: => Double): Int = {
      if (math.ceil(priceB) < priceA) n
      else countVisits(n + 1, priceA + ticket, priceB + memberTicket, memberTicket * perc)
    }

    countVisits(0, 0, card, ticket * perc)
  }

  //another movie
  def movie2(card: Int, ticket: Int, perc: Double): Int =
    Iterator
      .from(1)
      .scanLeft(card.toDouble) { case (total, i) => total + ticket * math.pow(perc, i) }
      .zipWithIndex
      .takeWhile { case (total, i) => total.ceil >= ticket * i }
      .size

  def triangle(row: String): String = {
    val colours = Set('R', 'G', 'B')
    if (row.length == 1) row
    else triangle(row.zip(row.tail).map {
      case (c, d) if c == d => c
      case (c, d) => (colours - c - d).head
    }.mkString)
  }

  def isPalindrome(words: String): List[Boolean] = {
    words.split(", ").map(x => if (x.reverse == x) true else false).toList
  }

  def seatsInTheater(totCols: Int, totRows: Int, col: Int, row: Int): Int = {
    (totCols - col + 1) * (totRows - row)
  }

  //how many cows and chickens in the farm
  def farm(heads: Int, legs: Int) = {

    val cows = legs / 2 - heads
    val chickens = heads - cows

    if (heads > 0 && legs > 0 && heads < 1000 && legs < 1000 && (cows >= 0 && chickens >= 0) && (heads % 2 == 0 && legs % 2 == 0))
      (chickens, cows)
    else if (heads == 0 && legs == 0)
      (0, 0)
    else
      "no solutions"
  }

  def isFlush(cards: List[String]): Boolean = {
    cards.map(_.last).distinct.size == 1
  }

  def countSmileys(vec: Vector[String]) = {
    vec.count(_.matches("[:;][~-]?[)D]"))
  }

  //multiples items in a list together
  def grow(xs: List[Long]): Long = xs.reduceLeft(_ * _)

  def grow2(xs: List[Long]): Long = xs.product

  //odd friggin count
  def oddCount(n: Long): Long = n / 2

  def centuryFromYear(year: Int) = {
    if (year < 100)
      10
    else if (year % 100 == 0)
      year / 100
    else
      year / 100 + 1
  }

  //or do it like this idiot
  def centuryFromYear2(year: Int): Int = (year + 99) / 100

  //adding up smallest ints in an unordered array until they are more than k
  def minimumSteps(numbers: Array[Int], k: Int) = {
    numbers.sorted.scanLeft(0)(_ + _).takeWhile(_ < k).size - 1
  }

  def sortByLength(arr: Array[String]) = {
    arr.sortBy(_.length)
  }

  //math stuff for divergent number sequence 1/3n+1
  def seriesSum(n: Int): String = {
    (0 until n).foldLeft(0.0)((acc, x) => acc + 1.0 / (1.0 + (x * 3.0))).formatted("%1.2f")
  }

  def number(busStops: List[(Int, Int)]) = {
    busStops.map(x => x._1 - x._2).sum
  }

  def duplicateEncode(word: String) = {
    word.toLowerCase.map(
      c =>
        if (word.toLowerCase.count(_ == c) <= 1) "("
        else ")"
    ).mkString
  }

  def duplicateEncoded(word: String): String = {
    val charCount = word.toLowerCase.groupBy(identity).mapValues(_.size)
    word.toLowerCase.map(
      c =>
        if (charCount(c) > 1) '('
        else ')'
    )
  }

  //group number having commas in the right place
  def groupByCommas(n: Int) = {
    n.toString.reverse.grouped(3).mkString(",").reverse
  }

  //how many times need to break chocolate to have square of 1x1
  def breakingChocolate(n: Int, m: Int): Int = 0 max n * m - 1

  def createPhoneNumber(numbers: Seq[Int]) = {
    val first = numbers.mkString.substring(0, 3)
    val second = numbers.mkString.substring(3, 6)
    val third = numbers.mkString.substring(6, 9)
    s"($first) $second-$third"
  }

  //encrypt a string by shifting the letter the Int provided along in the alphabet and then making every other letter a different case and reversing the string

  def playPass(str: String, n: Int): String = {
    val alphabet = 'a' to 'z'
    str.map {
      case c if c.isLetter => alphabet((alphabet.indexOf(c.toLower) + n) % 26)
      case c if c.isDigit => s"${9 - c.asDigit}".head
      case c => c
    }
      .zipWithIndex
      .map {
        case (c, i) if i % 2 == 0 => c.toUpper
        case (c, _) => c.toLower
      }
      .reverse
      .mkString
  }

  def highest(n: List[String]): Int = {
    n.map(_.toInt).max
  }

  def stringSort(str: List[String]) = {
    str.sortBy(_.length).reverse.mkString(" ")
  }

  def sortByLast(str: List[String]) = {
    str.sortBy(_.last).mkString(" ")
  }

  //takes 2 arrays of words and check array2 so see if contents of array1 are inside and then returns alphabetical order if are
  def inArray(array1: Array[String], array2: Array[String]) = {
    array1.filter(x => array2.exists(_.contains(x))).distinct.sorted.mkString(" ")
  }

  //order a string, each word in string has number and order by number
  def order(str: String): String = {
    str
      .split(' ')
      .sortBy(_.find(_.isDigit)).mkString(" ")
  }

  //each letter is 1 to 26, add up each word and return highest
  def high(str: String) = {
    str.split(" ").maxBy(_.map(_.toInt - 96).sum)
  }

  //returns string with zipCode:StreetName/Number if zipcode matches
  def travel(r: String, zipcode: String) = {
    val adresses = r.split(",").collect { case address if address.endsWith(s" $zipcode") => address.split(" ", 2) }
    val numbers = adresses.map(_ (0)).mkString(",")
    val streets = adresses.map(_ (1).dropRight(zipcode.size + 1)).mkString(",")
    s"$zipcode:$streets/$numbers"
  }
//returns number when i = +1, d = -1, s= square number, o = return that number, returns sum of all from string of chars
  def parse(data: String) = {
    data.foldLeft(0, List[Int]()) {
      case ((v, out), 'i') => (v+1, out)
      case ((v, out), 'd') => (v-1, out)
      case ((v, out), 's') => (v*v, out)
      case ((v, out), 'o') => (v,   out :+ v)
    }._2
  }

  //each letter of name = 1 to 26. length of name added to sum of letters weight in Array corresponds to order of names, multiply weight by sum of name
  def nthRank(str: String, we: Array[Int], n: Int): String = {
    val alphabet = ('a' to 'z').zipWithIndex.toMap.mapValues(_+1)
    str match {
      case "" => "No participants"
      case _ =>
        str
          .split(",")
          .zip(we)
          .sortBy { case (name, weight) => (-weight * (name.size + name.toLowerCase.map(alphabet).sum), name) }
          .map(_._1)
          .lift(n-1)
          .getOrElse("Not enough participants")
    }
  }
//encrypt, 1st letter into ASCII, 2nd letter swaps with last, add the rest in the middle
  def encryptThis(text: String): String = {
    text.split(" ")
      .map {
        case word if word.size == 1 => word(0).toInt
        case word if word.size == 2 => s"${word(0).toInt}${word.tail}"
        case word => s"${word(0).toInt}${word.last}${word.drop(2).dropRight(1)}${word(1)}"
      }.mkString(" ")
  }

  def decipherThis(str: String) = {
    val splitStr = str.split(" ")
    val converted = splitStr.map(_.replaceAll("[a-zA-Z]", "").toInt).map(_.toChar).toList
    val listStr = splitStr.map(_.replaceAll("[0-9]", "")).toList
    val newStr = converted.zip(listStr).map(x => s"${x._1}${x._2}").mkString(" ")

    newStr.split(" ").map {
      case w if w.size <= 2 => w
      case w if w.size > 2 => s"${w(0)}${w.last}${w.drop(2).dropRight(1)}${w(1)}"
    }.mkString(" ")
  }

  //takes a string and turns any Arabic numerals into Roman numerals
  def encodeRoman(str: String): String = {
    val numerals = List(
      List("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"),
      List("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"),
      List("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
    )
    str.split(" ").toList.map {
      case x if x.matches("[0]{1}") => "nulla"
      case x if x.matches("[0-9]+")  =>
        val num = Integer.parseInt(x)
        "M" * (num / 1000) + numerals.head(num % 1000 / 100) + numerals(1)(num % 100 / 10) + numerals(2)(num % 10)
      case x => x
    }.mkString(" ")
  }

  //adds letters together as if were numbers 1 to 26 an returns the corresponding letter in the alphabet
  def addLetters(letters: List[Char]) = {
    val abc = 'z' +: ('a' to 'y')
    abc(letters.map(abc.indexOf).sum % 26)
  }


}

