package com.garallex.stocks.fundamental

import com.garallex.stocks.domain.Stock

case class Roe(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = stock.roe.map(_ > BigDecimal("0.12"))
}
