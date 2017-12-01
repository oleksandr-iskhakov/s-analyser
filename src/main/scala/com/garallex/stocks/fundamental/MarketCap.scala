package com.garallex.stocks.fundamental

import com.garallex.stocks.domain.Stock

case class MarketCap(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = stock.marketCap.map(_ >= BigDecimal("10000000000"))
}
