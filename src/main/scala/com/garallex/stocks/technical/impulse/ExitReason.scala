package com.garallex.stocks.technical.impulse

object ExitReason extends Enumeration {
  type ExitReason = Value
  val StopLoss, TakeProfit = Value
}
