package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class MarketCap(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = stock.marketCap.map(_ >= BigDecimal("10000000000"))
}
