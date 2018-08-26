package com.garallex.stocks.datasource.apisource

import java.time.LocalDate

import com.garallex.stocks.domain.ATR
import com.garallex.stocks.domain.Orderings._
import org.json4s.native.JsonMethods.parse

class ApiATRLoader extends ApiLoader[ATR] {
  override protected val apiFunction: String = "ATR"

  override protected def parseFromJson(rawJsonString: String): Vector[ATR] = {
    val jSon = parse(rawJsonString, useBigDecimalForDouble = true, useBigIntForLong = true)
    val seriesJson = (jSon \ "Technical Analysis: ATR").values.asInstanceOf[Map[String, Map[String, String]]]

    seriesJson
      .map { case (date, values) =>
        ATR(
          date = LocalDate.parse(date.take(10)),
          atr = BigDecimal(values("ATR"))
        )
      }
      .toVector
      .sortBy(_.date)
  }
}
