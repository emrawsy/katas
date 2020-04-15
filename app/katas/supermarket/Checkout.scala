package katas.supermarket

class Checkout(priceRules: PriceRules) {

 var total = 0

 def scan(itemCode: String): Unit = {
  total += priceRules.priceOf(itemCode)
 }
}

