package com.garallex.stocks.fundamental

import com.garallex.stocks.domain.Stock

case class IntrinsicValueWa(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = (stock.actualPrice, stock.intrinsicValueWa) match {
    case (Some(price), Some(iv)) => Some(price <= iv * BigDecimal(0.8))
    case _ => None
  }

}
