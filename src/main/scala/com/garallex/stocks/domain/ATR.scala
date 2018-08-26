package com.garallex.stocks.domain

import java.time.LocalDate

case class ATR(override val date: LocalDate, atr: BigDecimal) extends DateBased(date)
