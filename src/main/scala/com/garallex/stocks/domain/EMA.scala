package com.garallex.stocks.domain

import java.time.LocalDate

case class EMA(override val date: LocalDate, ema: BigDecimal) extends DateBased(date)
