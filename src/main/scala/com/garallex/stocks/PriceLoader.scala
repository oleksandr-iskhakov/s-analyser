package com.garallex.stocks

import java.net.URL
import java.time.LocalDate

import com.garallex.stocks.domain.Orderings._
import com.garallex.stocks.domain.Price
import org.json4s.native.JsonMethods.parse

import scala.collection.immutable.SortedMap
import scala.io.Source

object PriceLoader {

  def load(ticker: String): SortedMap[LocalDate, Price] = {
    //    val url: URL = new URL(s"https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=$ticker&apikey=63Z6TXYSMOCUZ0N7&outputsize=compact")
    val url: URL = new URL(s"file:///Users/olek/Work/StockMarket/s-analyser/testFiles/price_msft_compact.json")
    val rawJsonString = Source.fromInputStream(url.openConnection().getInputStream).mkString

    val jSon = parse(rawJsonString, useBigDecimalForDouble = true, useBigIntForLong = true)
    val seriesJson = (jSon \ "Time Series (Daily)").values.asInstanceOf[Map[String, Map[String, String]]]

    val map = seriesJson.map { case (date, values) =>
      LocalDate.parse(date) -> Price(
        open = BigDecimal(values("1. open")),
        high = BigDecimal(values("2. high")),
        low = BigDecimal(values("3. low")),
        close = BigDecimal(values("4. close")),
        volume = BigDecimal(values("5. volume")))
    }
    SortedMap(map.toSeq: _*)
  }
}
