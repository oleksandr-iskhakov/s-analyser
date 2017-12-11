package com.garallex.stocks.technical

import java.time.LocalDate

import com.garallex.stocks.datasource.PriceSource
import com.garallex.stocks.technical.breakout.Breakout
import com.typesafe.scalalogging.LazyLogging

import scala.util.{Failure, Success, Try}

class SetupScanner() extends LazyLogging {
  private def processForTicker(ticker: String, lastExpectedDate: LocalDate) = Try {
    val priceSource = new PriceSource
    val price = Try(priceSource.load(ticker, lastExpectedDate)).get
    priceSource.close()
    logger.info(s"$ticker: Breakout executed")
    new Breakout(price).screen()
  } match {
    case Success(result) => SetupScannerResult(ticker, SetupType.Breakout, Left(result))
    case Failure(e) => SetupScannerResult(ticker, SetupType.Breakout, Right(e.getMessage))
  }

  def scan(tickers: List[String], lastExpectedDate: LocalDate): List[SetupScannerResult] =
    tickers.map(ticker => processForTicker(ticker, lastExpectedDate))
}