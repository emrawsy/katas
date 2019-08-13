package katas

import org.scalatest.{FreeSpec, Matchers}

class FizzBuzzSpec extends FreeSpec with Matchers {

  "FizzBuzz" - {
    "when ran" - {
      "should return Fizz if number is divisable by 3" in {
        FizzBuzz.getResult(3) should be("fizz")
        FizzBuzz.getResult(6) should be("fizz")
      }
      "should return Buzz if number is divisable by 5" in {
        FizzBuzz.getResult(5) should be("buzz")
        FizzBuzz.getResult(10) should be("buzz")
      }
      "should return Fizzbuzz if number is divisable by 3 and 5" in {
        FizzBuzz.getResult(15) should be("fizzbuzz")
        FizzBuzz.getResult(30) should be("fizzbuzz")
      }
      "should return the number if none of requirements apply" in {
        FizzBuzz.getResult(1) should be("1")
        FizzBuzz.getResult(4) should be("4")
      }
    }
  }

}
