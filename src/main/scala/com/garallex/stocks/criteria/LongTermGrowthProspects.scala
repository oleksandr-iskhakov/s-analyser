package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class LongTermGrowthProspects(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = stock.longTermGrowth match {
    case Some(ltg) => Some(ltg > 0.1)
    case _ => None
  }
}
