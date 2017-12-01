package com.garallex.stocks.fundamental

import com.garallex.stocks.domain.Stock

case class NetCurrentAssetsToLongTermDebt(stock: Stock) extends Criteria {
  override def result: Option[Boolean] = (stock.netCurrentAssets, stock.longTermDebt) match {
    case (Some(nca), Some(ltd)) => Some(nca >= ltd)
    case _ => None
  }
}
