package com.garallex.stocks.domain

import java.time.LocalDate

case class MACD(override val date: LocalDate, macd: BigDecimal, macdSignal: BigDecimal, macdHist: BigDecimal) extends DateBased(date)
