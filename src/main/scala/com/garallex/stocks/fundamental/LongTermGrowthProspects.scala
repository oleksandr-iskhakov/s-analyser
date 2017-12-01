package com.garallex.stocks.fundamental

import com.garallex.stocks.domain.Stock

case class LongTermGrowthProspects(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = stock.longTermGrowth match {
    case Some(longTermGrowthRate) => Some(longTermGrowthRate > 0.1)
    case _ => None
  }
}
