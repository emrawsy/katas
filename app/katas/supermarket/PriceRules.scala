package katas.supermarket

trait PriceRules {
  def priceOf(itemCode: String): Int
}
