package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class IntrinsicValueAdamKhooOnFreeCashFlow(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = (stock.actualPrice, stock.intrinsicValueAdamKhooOnFreeCashFlow) match {
    case (Some(price), Some(iv)) => Some(price <= iv * BigDecimal(0.8))
    case _ => None
  }

}
