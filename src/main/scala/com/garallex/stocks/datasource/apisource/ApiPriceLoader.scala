package com.garallex.stocks.datasource.apisource

import java.net.URL
import java.time.LocalDate

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.Utils.exceptionToString
import com.garallex.stocks.datasource.apisource.FetchType.FetchType
import com.garallex.stocks.domain.Candle
import com.garallex.stocks.domain.Orderings._
import com.typesafe.scalalogging.LazyLogging
import org.json4s.native.JsonMethods.parse

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object ApiPriceLoader extends LazyLogging {

  private final val ApiKey = "63Z6TXYSMOCUZ0N7"
  private final val RetryCount = 5
  private final val RetryPauseMs = 3000

  private def parseFromJson(rawJsonString: String): List[Candle] = {
    val jSon = parse(rawJsonString, useBigDecimalForDouble = true, useBigIntForLong = true)
    val seriesJson = (jSon \ "Time Series (Daily)").values.asInstanceOf[Map[String, Map[String, String]]]

    seriesJson
      .map { case (date, values) =>
        Candle(
          date = LocalDate.parse(date),
          open = BigDecimal(values("1. open")),
          high = BigDecimal(values("2. high")),
          low = BigDecimal(values("3. low")),
          close = BigDecimal(values("4. close")),
          volume = BigDecimal(values("5. volume")))
      }.toList
      .sortBy(_.date)
  }

  @tailrec
  private def fetchAndPArseWithRetry(url: URL, retryCount: Int, retryIntervalMs: Int): Either[PriceSeries, Throwable] =
    Try(Source.fromInputStream(url.openConnection().getInputStream).mkString) match {
      case Success(rawJsonString) =>
        Try(parseFromJson(rawJsonString)) match {
          case Success(priceSeries) => Left(priceSeries)
          case Failure(e) if retryCount > 0 =>
            logger.error(s"Error in fetchAndPArseWithRetry on retry number $retryCount", e)
            Thread.sleep(retryIntervalMs)
            fetchAndPArseWithRetry(url, retryCount - 1, retryIntervalMs)
        }
      case Failure(e) if retryCount > 0 =>
        logger.error(s"Error in fetchAndPArseWithRetry on retry number $retryCount", e)
        Thread.sleep(retryIntervalMs)
        fetchAndPArseWithRetry(url, retryCount - 1, retryIntervalMs)
      case Failure(e) => Right(e)
    }

  //  @tailrec
  //  private def fetchWithRetry(url: URL, retryCount: Int, retryIntervalMs: Int): Either[String, Throwable] =
  //    Try(Source.fromInputStream(url.openConnection().getInputStream).mkString) match {
  //      case Success(result) =>
  //        Left(result)
  //      case Failure(_) if retryCount > 0 =>
  //        Thread.sleep(retryIntervalMs)
  //        fetchWithRetry(url, retryCount - 1, retryIntervalMs)
  //      case Failure(e) => Right(e)
  //    }

  private def formUrl(ticker: String, fetchType: FetchType): URL = {
    val outputSize = fetchType match {
      case FetchType.Full => "full"
      case FetchType.Compact => "compact"
    }
    new URL(s"https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=$ticker&apikey=$ApiKey&outputsize=$outputSize")
  }

  def fetch(ticker: String, fetchType: FetchType): PriceSeries = {
    val url = formUrl(ticker, fetchType)
    fetchAndPArseWithRetry(url, RetryCount, RetryPauseMs) match {
      case Left(priceSeries) => priceSeries
      case Right(e) =>
        logger.info(s"Couldn't fetch from API for $ticker after $RetryCount retries:\n${exceptionToString(e)}")
        List()
    }
  }
}
