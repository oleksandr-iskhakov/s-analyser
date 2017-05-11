package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class CurrentRatioActive(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = stock.currentRatio.map(_ >= BigDecimal("1.5"))
}
