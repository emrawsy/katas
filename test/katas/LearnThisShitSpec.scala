package katas

import org.scalatest.{FunSpec, MustMatchers}

class LearnThisShitSpec extends FunSpec with MustMatchers {
  describe("LearnThisShit.accum") {
    it("should work with example tests") {
      assertResult("Z-Pp-Ggg-Llll-Nnnnn-Rrrrrr-Xxxxxxx-Qqqqqqqq-Eeeeeeeee-Nnnnnnnnnn-Uuuuuuuuuuu") {
        LearnThisShit.accum("ZpglnRxqenU")
      }
      assertResult("N-Yy-Fff-Ffff-Sssss-Gggggg-Eeeeeee-Yyyyyyyy-Yyyyyyyyy-Llllllllll-Bbbbbbbbbbb") {
        LearnThisShit.accum("NyffsGeyylB")
      }
      assertResult("M-Jj-Ttt-Kkkk-Uuuuu-Bbbbbb-Ooooooo-Vvvvvvvv-Qqqqqqqqq-Rrrrrrrrrr-Uuuuuuuuuuu") {
        LearnThisShit.accum("MjtkuBovqrU")
      }
      assertResult("E-Vv-Iii-Dddd-Jjjjj-Uuuuuu-Nnnnnnn-Oooooooo-Kkkkkkkkk-Mmmmmmmmmm-Mmmmmmmmmmm") {
        LearnThisShit.accum("EvidjUnokmM")
      }
      assertResult("H-Bb-Iii-Dddd-Eeeee-Vvvvvv-Bbbbbbb-Xxxxxxxx-Nnnnnnnnn-Cccccccccc-Ccccccccccc") {
        LearnThisShit.accum("HbideVbxncC")
      }
    }
  }

  describe("LearnThisShit.isSquare") {
    it("should work with example tests") {
      val tests = List(
        (-1, false),
        (0, true),
        (3, false),
        (4, true),
        (25, true)
      )
      tests.foreach {
        case (input, expected) =>
          LearnThisShit.isSquare(input) mustEqual expected

      }
    }
  }

  describe("LearnThisShit.evaporator") {
    it("when given (10,10,10) should return 22") {
      LearnThisShit.evaporator(10, 10, 10) mustEqual 22
    }
  }

  describe("LearnThisShit.findMissingLetter") {
    assert(LearnThisShit.findMissingLetter(Array('a', 'b', 'c', 'd', 'f')) === 'e')
    assert(LearnThisShit.findMissingLetter(Array('O', 'Q', 'R', 'S')) === 'P')
  }

  describe("LearnThisShit.count") {
    it("count chars should work with example tests") {
      assert(LearnThisShit.countChars("aba") == Map[Char, Int]('a' -> 2, 'b' -> 1))
      assert(LearnThisShit.countChars("") == Map[Char, Int]())
    }
  }

  describe("LearnThisShit.duplicateCount") {
    it("duplicate count should work with these tests") {
      assert(LearnThisShit.duplicateCount("abcde") == 0)
      assert(LearnThisShit.duplicateCount("abcdea") == 1)
      assert(LearnThisShit.duplicateCount("indivisibility") == 1)
      assert(LearnThisShit.duplicateCount("aBbCc11") == 3)
    }
  }

  describe("LearnThisShit.meeting") {
    it("meeting should work with these") {
      assert(LearnThisShit.meeting("Alexis:Wahl;John:Bell;Victoria:Schwarz;Abba:Dorny;Grace:Meta;Ann:Arno;Madison:STAN;Alex:Cornwell;Lewis:Kern;Megan:Stan;Alex:Korn") ==
        ("(ARNO, ANN)(BELL, JOHN)(CORNWELL, ALEX)(DORNY, ABBA)(KERN, LEWIS)(KORN, ALEX)(META, GRACE)(SCHWARZ, VICTORIA)(STAN, MADISON)(STAN, MEGAN)(WAHL, ALEXIS)"))
      assert(LearnThisShit.meeting("John:Gates;Michael:Wahl;Megan:Bell;Paul:Dorries;James:Dorny;Lewis:Steve;Alex:Meta;Elizabeth:Russel;Anna:Korn;Ann:Kern;Amber:Cornwell") ==
        ("(BELL, MEGAN)(CORNWELL, AMBER)(DORNY, JAMES)(DORRIES, PAUL)(GATES, JOHN)(KERN, ANN)(KORN, ANNA)(META, ALEX)(RUSSEL, ELIZABETH)(STEVE, LEWIS)(WAHL, MICHAEL)"))

    }
  }

//  describe("LearnThisShit.decode") {
//    assert(LearnThisShit.decode("I") === 1)
//    assert(LearnThisShit.decode("III") === 3)
//    assert(LearnThisShit.decode("IV") === 4)
//    assert(LearnThisShit.decode("XXI") === 21)
//    assert(LearnThisShit.decode("MDCLXVI") === 1666)
//    assert(LearnThisShit.decode("MCMXC") === 1990)
//    assert(LearnThisShit.decode("MMVIII") === 2008)
//  }


  describe("LearnThisShit.encodeRoman") {
    it("should encode any integer given into roman numerals") {
      assert(LearnThisShit.encodeRoman(1990) === "MCMXC")
      assert(LearnThisShit.encodeRoman(2985) === "MMCMLXXXV")
      assert(LearnThisShit.encodeRoman(511) === "DXI")
      assert(LearnThisShit.encodeRoman(900) === "CM")
      assert(LearnThisShit.encodeRoman(300) === "CCC")
      assert(LearnThisShit.encodeRoman(2) === "II")
      assert(LearnThisShit.encodeRoman(21) === "XXI")
    }
  }

}
