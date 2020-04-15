package katas

import org.scalatest.{FreeSpec, Matchers}

class FizzBuzzSpec extends FreeSpec with Matchers {
val fizzBuzz = new FizzBuzz
  "FizzBuzz" - {
    "when ran" - {
      "should return Fizz if number is divisable by 3" in {
        fizzBuzz.getResult(3) should be("fizz")
        fizzBuzz.getResult(6) should be("fizz")
      }
      "should return Buzz if number is divisable by 5" in {
        fizzBuzz.getResult(5) should be("buzz")
        fizzBuzz.getResult(10) should be("buzz")
      }
      "should return Fizzbuzz if number is divisable by 3 and 5" in {
        fizzBuzz.getResult(15) should be("fizzbuzz")
        fizzBuzz.getResult(30) should be("fizzbuzz")
      }
      "should return the number if none of requirements apply" in {
        fizzBuzz.getResult(1) should be("1")
        fizzBuzz.getResult(4) should be("4")
      }
    }
  }

}
