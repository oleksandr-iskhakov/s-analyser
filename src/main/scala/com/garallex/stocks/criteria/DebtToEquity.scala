package com.garallex.stocks.criteria

import com.garallex.stocks.domain.Stock

case class DebtToEquity(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = stock.debtToEquity.map(_ < BigDecimal("0.5"))
}
