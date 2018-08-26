package com.garallex.stocks.domain

import java.time.LocalDate

case class RSI(override val date: LocalDate, rsi: BigDecimal) extends DateBased(date)
