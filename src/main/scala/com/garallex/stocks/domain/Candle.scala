package com.garallex.stocks.domain

import java.time.LocalDate

case class Candle(override val date: LocalDate, open: BigDecimal, high: BigDecimal, low: BigDecimal, close: BigDecimal, volume: BigDecimal)
  extends DateBased(date) {
  lazy val topOfCandleBody: BigDecimal = open.max(close)
  lazy val bottomOfCandleBody: BigDecimal = open.min(close)

  override def toString: String = s"D:$date O:$open H:$high L:$low C:$close V:$volume"
}
