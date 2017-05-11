package com.garallex.stocks.criteria

import com.garallex.stocks.Stock

abstract class Criteria(stock: Stock) {
  def result: Option[Boolean]

  override def toString: String = result match {
    case Some(true) => "PASSED"
    case Some(false) => "NOT PASSED"
    case None => "NO DATA"
  }
}

