package com.garallex.stocks.domain

import java.time.LocalDate

case class Stochastic(date: LocalDate, slowK: BigDecimal, slowD: BigDecimal)
