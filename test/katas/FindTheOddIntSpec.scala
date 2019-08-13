package katas

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FreeSpec, MustMatchers}

class FindTheOddIntSpec extends FreeSpec with MustMatchers with TableDrivenPropertyChecks {

  //  test("Fixed tests") {
  //    val fixedTests = Table[Seq[Int], Int](
  //      ("xs", "expected"),
  //      (List(20,1,-1,2,-2,3,3,5,5,1,2,4,20,4,-1,-2,5), 5),
  //      (List(1,1,2,-2,5,2,4,4,-1,-2,5), -1),
  //      (List(20,1,1,2,2,3,3,5,5,4,20,4,5), 5),
  //      (List(10), 10),
  //      (List(1,1,1,1,1,1,10,1,1,1,1), 10),
  //      (List(5,4,3,2,1,5,4,3,2,10,10), 1),
  //    )
  //    forAll(fixedTests) { findOdd(_) mustEqual _ }
  //  }
  "order(\"is2 Thi1s T4est 3a\")" - {
    "return \"Thi1s is2 3a T4est\"" in {
      Text.order("is2 Thi1s T4est 3a") mustEqual ("Thi1s is2 3a T4est")
    }
  }


}
