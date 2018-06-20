package com.garallex.stocks.datasource.apisource

import java.time.LocalDate

import com.garallex.stocks.domain.Orderings._
import com.garallex.stocks.domain.Stochastic
import org.json4s.native.JsonMethods.parse

object ApiStochasticLoader extends ApiLoader[Stochastic] {
  override protected val apiFunction: String = "STOCH"

  override protected def parseFromJson(rawJsonString: String): Vector[Stochastic] = {
    val jSon = parse(rawJsonString, useBigDecimalForDouble = true, useBigIntForLong = true)
    val seriesJson = (jSon \ "Technical Analysis: STOCH").values.asInstanceOf[Map[String, Map[String, String]]]

    seriesJson
      .map { case (date, values) =>
        Stochastic(
          date = LocalDate.parse(date.take(10)),
          slowK = BigDecimal(values("SlowK")),
          slowD = BigDecimal(values("SlowD"))
        )
      }.toVector
      .sortBy(_.date)
  }
}
