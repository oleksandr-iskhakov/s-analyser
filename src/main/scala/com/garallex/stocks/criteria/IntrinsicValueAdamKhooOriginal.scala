package com.garallex.stocks.criteria

import com.garallex.stocks.domain.Stock

case class IntrinsicValueAdamKhooOriginal(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = (stock.actualPrice, stock.intrinsicValueAdamKhooOriginal) match {
    case (Some(price), Some(iv)) => Some(price <= iv * BigDecimal(0.8))
    case _ => None
  }

}
