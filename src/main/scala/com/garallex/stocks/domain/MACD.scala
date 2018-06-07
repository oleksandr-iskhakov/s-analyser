package com.garallex.stocks.domain

import java.time.LocalDate

case class MACD(date: LocalDate, macd: BigDecimal, macdSignal: BigDecimal, macdHist: BigDecimal)
