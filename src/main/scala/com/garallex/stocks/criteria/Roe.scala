package com.garallex.stocks.criteria

import com.garallex.stocks.domain.Stock

case class Roe(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = stock.roe.map(_ > BigDecimal("0.12"))
}
