package katas

import org.scalatest.{FreeSpec, Matchers, MustMatchers}

class NinetyNineProblemsSpec extends FreeSpec with MustMatchers {

  "NinetyNineProblems should" - {
    "The string kata must be able to" - {
      "return a string of numbers separated by a comma if correct data is given" in {
        NinetyNineProblems.Add("1", "2") mustEqual "1, 2"
      }
      "return 0 if given an empty string" in {
        NinetyNineProblems.Add("", "") mustEqual 0

      }
    }
    "convert to int" in {
      NinetyNineProblems.convertToInt('c') mustEqual 3
    }
    "convert string to list of ints" in {
      NinetyNineProblems.stringToChars("abc") mustEqual List(1,2,3)
      NinetyNineProblems.stringToChars("def") mustEqual List(4,5,6)
    }
    "return correct length keyString" - {
      "when given key same length as string" in {
        NinetyNineProblems.keyString("h", 1) mustEqual "1"
      }
      "when given key shorter than input" in {
        NinetyNineProblems.keyString("masterpiece", 1939) mustEqual "19391939193"
      }
    }
  }
}
