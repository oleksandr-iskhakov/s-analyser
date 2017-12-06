package com.garallex.stocks.technical

import java.time.LocalDate

import com.garallex.stocks.datasource.PriceSource
import com.garallex.stocks.technical.SetupType.SetupType
import com.garallex.stocks.technical.breakout.Breakout

class SetupScanner(priceSource: PriceSource) {
  def scan(tickers: List[String], lastExpectedDate: LocalDate): Map[String, SetupType] =
    tickers.map(ticker => {
      val price = priceSource.load(ticker, lastExpectedDate)
      val result = new Breakout(price.tail, BigDecimal(0.8), BigDecimal(0.8)).screen()
      ticker -> result
    })
      .filter { case (_, result) => result }
      .toMap
      .mapValues(_ => SetupType.Breakout)
}
