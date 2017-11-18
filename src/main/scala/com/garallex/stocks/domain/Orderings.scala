package com.garallex.stocks.domain

import java.time.LocalDate

object Orderings {
  implicit val localDateOrdering: Ordering[LocalDate] = Ordering.fromLessThan(_ isBefore _)
}
