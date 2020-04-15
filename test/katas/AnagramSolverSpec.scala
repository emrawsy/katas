package katas

import org.scalatest.{FreeSpec, MustMatchers}

class AnagramSolverSpec extends FreeSpec with MustMatchers {

  "Anagram solver must" - {
    "when given a string return all possible correct solutions" in {
      AnagramSolver.solve("lehp") mustEqual "help"
      AnagramSolver.solve("sinks") mustEqual "skins"
      AnagramSolver.solve("crepitus") mustEqual "cuprites pictures piecrust"
    }
    "when given something other than a string it should fail" in {

    }
  }

}
