package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class PriceToEarnings(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = stock.peRatio.map(_ < BigDecimal("0.15"))
}
