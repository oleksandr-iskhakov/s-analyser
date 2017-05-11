package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class Roe(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = stock.roe.map(_ > BigDecimal("0.12"))
}
