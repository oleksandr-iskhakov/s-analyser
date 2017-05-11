package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class DebtToEquity(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = stock.debtToEquity.map(_ < BigDecimal("0.5"))
}
