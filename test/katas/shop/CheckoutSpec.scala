package katas.shop

import org.scalatest.{FreeSpec, MustMatchers}

class CheckoutSpec extends FreeSpec with MustMatchers {

  "Checkout" - {
    "when scan is called" - {
      "must return price of item" in {
        Checkout.scan(List("apple", "apple", "orange")) mustEqual (3.55)
      }
    }
  }

}
