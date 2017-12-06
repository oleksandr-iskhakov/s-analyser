package com.garallex.stocks.datasource.apisource

import java.net.URL
import java.time.LocalDate

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.datasource.apisource.FetchType.FetchType
import com.garallex.stocks.domain.Candle
import com.garallex.stocks.domain.Orderings._
import org.json4s.native.JsonMethods.parse

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.io.Source
import scala.util.{Failure, Success, Try}

object ApiPriceLoader {

  private val apiKey = "63Z6TXYSMOCUZ0N7"

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
      .sortBy(c => c.date)
  }

  @tailrec
  def fetchWithRetry(url: URL, retryCount: Int, retryIntervalMs: Int): Either[String, Throwable] =
    Try(Source.fromInputStream(url.openConnection().getInputStream).mkString) match {
      case Success(result) => Left(result)
      case Failure(_) if retryCount > 0 =>
        Thread.sleep(retryIntervalMs)
        fetchWithRetry(url, retryCount - 1, retryIntervalMs)
      case Failure(e) => Right(e)
    }

  def fetch(ticker: String, fetchType: FetchType): PriceSeries = {
    val outputSize = fetchType match {
      case FetchType.Full => "full"
      case FetchType.Compact => "compact"
    }
    val url: URL = new URL(s"https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=$ticker&apikey=$apiKey&outputsize=$outputSize")

    fetchWithRetry(url, 3, 1000) match {
      case Left(rawJsonString) => parseFromJson(rawJsonString)
      case Right(e) =>
        println(s"Couldn't fetch from API for $ticker with exception:\n$e")
        List()
    }
  }
}
