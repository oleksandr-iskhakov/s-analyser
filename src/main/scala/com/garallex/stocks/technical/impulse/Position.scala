package com.garallex.stocks.technical.impulse

import java.time.LocalDate

import com.garallex.stocks.technical.impulse.Direction.Direction
import com.garallex.stocks.technical.impulse.ExitReason.ExitReason

case class Position(entryDate: LocalDate,
                    entryPrice: BigDecimal,
                    direction: Direction,
                    size: Int,
                    r: BigDecimal,
                    exitDate: Option[LocalDate] = None,
                    exitPrice: Option[BigDecimal] = None,
                    exitReason: Option[ExitReason] = None) {

  lazy val profitLoss: BigDecimal = (exitPrice.getOrElse(BigDecimal(0)) - entryPrice) * size
  lazy val profitLossPercent: BigDecimal = (exitPrice.getOrElse(BigDecimal(0)) - entryPrice) / entryPrice

  override def toString = s"Profit/Loss: $profitLoss, Profit/Loss (%): $profitLossPercent, Exit Reason: $exitReason, Exit: $exitDate, Entry Price: $entryPrice, R: $r, "
}