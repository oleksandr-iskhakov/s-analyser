package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class LongTermDebtToNetIncomeAfterTax(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = (stock.longTermDebt, stock.netIncomeAfterTax) match {
    case (Some(ltd), Some(niat)) => Some(ltd / niat < 3)
    case _ => None
  }
}
