package com.garallex.stocks.fundamental

trait Criteria {
  def result: Option[Boolean]

  override def toString: String = result match {
    case Some(true) => "PASSED"
    case Some(false) => "NOT PASSED"
    case None => "NO DATA"
  }
}

