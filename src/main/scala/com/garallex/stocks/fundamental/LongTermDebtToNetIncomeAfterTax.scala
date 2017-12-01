package com.garallex.stocks.fundamental

import com.garallex.stocks.domain.Stock

case class LongTermDebtToNetIncomeAfterTax(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = (stock.longTermDebt, stock.netIncomeAfterTax) match {
    case (Some(longTermDebt), Some(netIncomeAfterTax)) => Some(longTermDebt / netIncomeAfterTax < 3)
    case _ => None
  }
}
