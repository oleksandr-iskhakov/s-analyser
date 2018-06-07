package com.garallex.stocks.datasource.apisource

import java.time.LocalDate

import com.garallex.stocks.domain.MACD
import com.garallex.stocks.domain.Orderings._
import org.json4s.native.JsonMethods.parse

object ApiMACDLoader extends ApiLoader[MACD] {
  override protected val apiFunction: String = "MACD"

  override protected def parseFromJson(rawJsonString: String): Vector[MACD] = {
    val jSon = parse(rawJsonString, useBigDecimalForDouble = true, useBigIntForLong = true)
    val seriesJson = (jSon \ "Technical Analysis: MACD").values.asInstanceOf[Map[String, Map[String, String]]]

    seriesJson
      .map { case (date, values) =>
        MACD(
          date = LocalDate.parse(date),
          macd = BigDecimal(values("MACD")),
          macdSignal = BigDecimal(values("MACD_Signal")),
          macdHist = BigDecimal(values("MACD_Hist"))
        )
      }.toVector
      .sortBy(_.date)
  }
}
