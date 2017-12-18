package com.garallex.stocks.technical

import java.time.LocalDate

import com.garallex.stocks.Utils.exceptionToString
import com.garallex.stocks.datasource.PriceSource
import com.garallex.stocks.technical.breakout.Breakout
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

class SetupScanner(portionSize: Int, priceSource: PriceSource, lastExpectedDate: LocalDate) extends LazyLogging {

  private def processForTicker(ticker: String): Future[SetupScannerResult] =
    priceSource
      .load(ticker, lastExpectedDate)
      .transform {
        case Success(priceSeries) => Try {
          logger.info(s"$ticker: Breakout executed")
          val result = new Breakout(priceSeries).screen()
          SetupScannerResult(ticker, SetupType.Breakout, Left(result))
        }
        case Failure(e) => Try(SetupScannerResult(ticker, SetupType.Breakout, Right(exceptionToString(e))))
      }

  def scan(tickers: List[String]): List[SetupScannerResult] =
    if (tickers.isEmpty)
      List()
    else {
      val scannerResultFutures = tickers.take(portionSize).map(ticker => processForTicker(ticker))
      val scannerResult = Await.result(Future.sequence(scannerResultFutures), 3 hours)
      scannerResult ++ scan(tickers.drop(portionSize))
    }
}