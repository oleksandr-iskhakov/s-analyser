package com.garallex.stocks.datasource.apisource

import java.time.LocalDate

import com.garallex.stocks.domain.Candle
import com.garallex.stocks.domain.Orderings._
import org.json4s.native.JsonMethods.parse

object ApiPriceLoader extends ApiLoader[Candle] {
  override protected val apiFunction: String = "TIME_SERIES_DAILY"

  override protected def parseFromJson(rawJsonString: String): Vector[Candle] = {
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
      }.toVector
      .sortBy(_.date)
  }

}
