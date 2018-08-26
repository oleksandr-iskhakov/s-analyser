package com.garallex.stocks.datasource.apisource

import java.net.URL

import com.garallex.stocks.Utils.exceptionToString
import com.garallex.stocks.datasource.apisource.FetchType.FetchType
import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

trait ApiLoader[T] extends LazyLogging {

  private final val ApiKey = "63Z6TXYSMOCUZ0N7"
  private final val RetryCount = 5
  private final val RetryPauseMs = 3000
  private var rawString: String = ""
  private var result: Vector[T] = Vector()

  protected def parseFromJson(rawJsonString: String): Vector[T]

  protected val apiFunction: String

  @tailrec
  private def fetchAndParseWithRetry(url: URL, retryCount: Int, retryIntervalMs: Int): Either[Vector[T], Throwable] =
    Try(Source.fromInputStream(url.openConnection().getInputStream).mkString) match {
      case Success(rawJsonString) =>
        rawString = rawJsonString
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
    logger.info(url.toString)
    fetchAndParseWithRetry(url, RetryCount, RetryPauseMs) match {
      case Left(series) =>
        result = series
        series

      case Right(e) =>
        logger.info(s"Couldn't fetch from API for $ticker after $RetryCount retries:\n${exceptionToString(e)}")
        Vector()
    }
  }

  def getRawString: String = if (rawString == "") throw new Exception("fetch was not called") else rawString
  def getResult: Vector[T] = result
}
