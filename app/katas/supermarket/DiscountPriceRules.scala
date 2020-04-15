package katas.supermarket

class DiscountPriceRules(pricing: Map[String, Price]) extends PriceRules {
  def priceOf(itemCode: String) = {
    pricing(itemCode).price
  }

}
