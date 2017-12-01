package com.garallex.stocks.fundamental

import com.garallex.stocks.domain.Stock

case class PriceToBook(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = stock.priceToBook.map(_ <= BigDecimal("1.5"))
}
