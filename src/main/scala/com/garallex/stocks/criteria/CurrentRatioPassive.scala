package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class CurrentRatioPassive(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = stock.currentRatio.map(_ >= BigDecimal("2"))
}
