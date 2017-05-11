package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class PriceToBook(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = stock.priceToBook.map(_ <= BigDecimal("1.5"))
}
