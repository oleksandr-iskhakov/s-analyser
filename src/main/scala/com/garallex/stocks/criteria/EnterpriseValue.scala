package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class EnterpriseValue(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = stock.enterpriseValue.map(_ >= BigDecimal("2000000000"))
}
