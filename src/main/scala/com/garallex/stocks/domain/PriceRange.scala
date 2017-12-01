package com.garallex.stocks.domain

case class PriceRange(level: BigDecimal, from: BigDecimal, to: BigDecimal) {
  assert(level >= from, "level must be higher or equal to from")
  assert(level <= to, "level must be lower or equal to to")
}
