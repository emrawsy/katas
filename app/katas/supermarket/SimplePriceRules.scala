package katas.supermarket

class SimplePriceRules(prices: Map[String, Int]) extends PriceRules {
  def priceOf(itemCode: String) = {
    prices(itemCode)
  }
}
