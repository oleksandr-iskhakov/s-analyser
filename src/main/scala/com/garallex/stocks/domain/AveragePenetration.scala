package com.garallex.stocks.domain

import java.time.LocalDate

case class AveragePenetration(override val date: LocalDate, penetration: BigDecimal) extends DateBased(date)
