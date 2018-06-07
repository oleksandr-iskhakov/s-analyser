package com.garallex.stocks.technical.impulse

import java.time.LocalDate

import com.garallex.stocks.technical.impulse.ExitReason.ExitReason

case class Position(entryDate: LocalDate,
                    entryPrice: BigDecimal,
                    size: Int,
                    exitDate: Option[LocalDate] = None,
                    exitPrice: Option[BigDecimal] = None,
                    exitReason: Option[ExitReason] = None)