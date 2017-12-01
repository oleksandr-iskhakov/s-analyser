package com.garallex.stocks.fundamental

import com.garallex.stocks.domain.Stock

case class IntrinsicValueGrahamUpdated(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = (stock.actualPrice, stock.intrinsicValueGrahamUpdated) match {
    case (Some(price), Some(iv)) => Some(price <= iv * BigDecimal(0.8))
    case _ => None
  }

}
