package com.garallex.stocks.domain

import java.time.LocalDate

case class Stochastic(override val date: LocalDate, slowK: BigDecimal, slowD: BigDecimal) extends DateBased(date)
