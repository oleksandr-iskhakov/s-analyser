package com.garallex.stocks.domain

case class Price(open: BigDecimal, high: BigDecimal, low: BigDecimal, close: BigDecimal, volume: BigDecimal) {
  override def toString: String = s"O:$open H:$high L:$low C:$close V: $volume"
}
