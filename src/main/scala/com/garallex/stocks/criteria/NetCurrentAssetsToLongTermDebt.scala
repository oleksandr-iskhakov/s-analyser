package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

case class NetCurrentAssetsToLongTermDebt(stock: Stock) extends Criteria(stock) {
  override def result: Option[Boolean] = (stock.netCurrentAssets, stock.longTermDebt) match {
    case (Some(nca), Some(ltd)) => Some(nca >= ltd)
    case _ => None
  }
}
