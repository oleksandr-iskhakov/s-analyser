package com.garallex.stocks.criteria

import com.garallex.stocks.domain.Stock

case class PriceToEarnings(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = stock.peRatio.map(_ < BigDecimal("15"))
}

