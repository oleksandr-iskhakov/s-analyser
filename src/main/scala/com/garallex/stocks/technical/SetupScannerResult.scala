package com.garallex.stocks.technical

import com.garallex.stocks.technical.SetupType.SetupType

case class SetupScannerResult(ticker: String, setupType: SetupType, result: Either[Boolean, String]) {
  override def toString = s"$ticker\t$setupType\t$result"
}
