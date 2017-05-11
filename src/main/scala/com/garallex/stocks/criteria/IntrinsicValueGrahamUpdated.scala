package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class IntrinsicValueGrahamUpdated(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = (stock.actualPrice, stock.intrinsicValueGrahamUpdated) match {
    case (Some(price), Some(iv)) => Some(price <= iv * BigDecimal(0.8))
    case _ => None
  }

}
