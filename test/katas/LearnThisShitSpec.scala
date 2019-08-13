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

  describe("LearnThisShit.mxdiflg") {
    it("mxdiflgt should work with these tests") {
      val a1 = List("hoqq", "bbllkw", "oox", "ejjuyyy", "plmiis", "xxxzgpsssa", "xxwwkktt", "znnnnfqknaz", "qqquuhii", "dvvvwz")
      val a2 = List("cccooommaaqqoxii", "gggqaffhhh", "tttoowwwmmww")
      val result = 13

      LearnThisShit.mxdiflg(a1, a2) mustEqual (result)
    }
    it("shoudl also work with these tests") {
      val a1 = List()
      val a2 = List("hello")
      val result = -1

      LearnThisShit.mxdiflg(a1, a2) mustEqual(result)

    }


  }

}
