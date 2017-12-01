package com.garallex.stocks.domain

case class Candle(open: BigDecimal, high: BigDecimal, low: BigDecimal, close: BigDecimal, volume: BigDecimal) {
  lazy val topOfCandleBody: BigDecimal = open.max(close)
  lazy val bottomOfCandleBody: BigDecimal = open.min(close)

  override def toString: String = s"O:$open H:$high L:$low C:$close V:$volume"
}
