package com.garallex.stocks.datasource.apisource

import java.time.LocalDate

import com.garallex.stocks.domain.EMA
import com.garallex.stocks.domain.Orderings._
import org.json4s.native.JsonMethods.parse

class ApiEMALoader extends ApiLoader[EMA] {
  override protected val apiFunction: String = "EMA"

  override protected def parseFromJson(rawJsonString: String): Vector[EMA] = {
    val jSon = parse(rawJsonString, useBigDecimalForDouble = true, useBigIntForLong = true)
    val seriesJson = (jSon \ "Technical Analysis: EMA").values.asInstanceOf[Map[String, Map[String, String]]]

    seriesJson
      .map { case (date, values) =>
        EMA(
          date = LocalDate.parse(date.take(10)),
          ema = BigDecimal(values("EMA")))
      }.toVector
      .sortBy(_.date)
  }

}
