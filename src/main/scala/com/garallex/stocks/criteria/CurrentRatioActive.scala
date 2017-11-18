package com.garallex.stocks.criteria

import com.garallex.stocks.domain.Stock

case class CurrentRatioActive(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = stock.currentRatio.map(_ >= BigDecimal("1.5"))
}
