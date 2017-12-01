package com.garallex.stocks.fundamental

import com.garallex.stocks.domain.Stock

case class EnterpriseValue(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = stock.enterpriseValue.map(_ >= BigDecimal("2000000000"))
}
