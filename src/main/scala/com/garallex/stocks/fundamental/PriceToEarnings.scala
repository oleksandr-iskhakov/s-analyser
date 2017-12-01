package com.garallex.stocks.fundamental

import com.garallex.stocks.domain.Stock

case class PriceToEarnings(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = stock.peRatio.map(_ < BigDecimal("15"))
}

