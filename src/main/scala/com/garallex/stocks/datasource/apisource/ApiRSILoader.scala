package com.garallex.stocks.datasource.apisource

import java.time.LocalDate

import com.garallex.stocks.domain.Orderings._
import com.garallex.stocks.domain.RSI
import org.json4s.native.JsonMethods.parse

class ApiRSILoader extends ApiLoader[RSI] {
  override protected val apiFunction: String = "RSI"

  override protected def parseFromJson(rawJsonString: String): Vector[RSI] = {
    val jSon = parse(rawJsonString, useBigDecimalForDouble = true, useBigIntForLong = true)
    val seriesJson = (jSon \ "Technical Analysis: RSI").values.asInstanceOf[Map[String, Map[String, String]]]

    seriesJson
      .map { case (date, values) =>
        RSI(
          date = LocalDate.parse(date.take(10)),
          rsi = BigDecimal(values("RSI"))
        )
      }.toVector
      .sortBy(_.date)
  }
}
