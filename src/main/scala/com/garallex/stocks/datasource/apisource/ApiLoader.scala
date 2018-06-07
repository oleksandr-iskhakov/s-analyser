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

trait ApiLoader[T] extends LazyLogging {

  private final val ApiKey = "63Z6TXYSMOCUZ0N7"
  private final val RetryCount = 5
  private final val RetryPauseMs = 3000

  protected def parseFromJson(rawJsonString: String): Vector[T]

  protected val apiFunction: String


  @tailrec
  private def fetchAndParseWithRetry(url: URL, retryCount: Int, retryIntervalMs: Int): Either[Vector[T], Throwable] =
    Try(Source.fromInputStream(url.openConnection().getInputStream).mkString) match {
      case Success(rawJsonString) =>
        Try(parseFromJson(rawJsonString)) match {
          case Success(priceSeries) => Left(priceSeries)
          case Failure(e) if retryCount > 0 =>
            logger.error(s"Error in fetchAndParseWithRetry on retry number $retryCount", e)
            Thread.sleep(retryIntervalMs)
            fetchAndParseWithRetry(url, retryCount - 1, retryIntervalMs)
        }
      case Failure(e) if retryCount > 0 =>
        logger.error(s"Error in fetchAndParseWithRetry on retry number $retryCount", e)
        Thread.sleep(retryIntervalMs)
        fetchAndParseWithRetry(url, retryCount - 1, retryIntervalMs)
      case Failure(e) => Right(e)
    }

  private def formUrl(ticker: String, fetchType: FetchType, parameters: Map[String, String]): URL = {
    val outputSize = fetchType match {
      case FetchType.Full => "full"
      case FetchType.Compact => "compact"
    }
    val paramStr = parameters
      .map { case (k, v) => "&" + k + "=" + v }
      .mkString

    val strUrl = s"https://www.alphavantage.co/query?function=$apiFunction&symbol=$ticker&apikey=$ApiKey&outputsize=$outputSize" + paramStr
    new URL(strUrl)
  }

  def fetch(ticker: String, fetchType: FetchType, parameters: Map[String, String] = Map()): Vector[T] = {
    val url = formUrl(ticker, fetchType, parameters)
    fetchAndParseWithRetry(url, RetryCount, RetryPauseMs) match {
      case Left(series) => series
      case Right(e) =>
        logger.info(s"Couldn't fetch from API for $ticker after $RetryCount retries:\n${exceptionToString(e)}")
        Vector()
    }
  }
}
