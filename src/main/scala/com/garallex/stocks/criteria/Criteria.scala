package com.garallex.stocks.criteria

trait Criteria {
  def result: Option[Boolean]

  override def toString: String = result match {
    case Some(true) => "PASSED"
    case Some(false) => "NOT PASSED"
    case None => "NO DATA"
  }
}

