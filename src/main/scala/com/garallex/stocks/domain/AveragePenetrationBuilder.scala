package com.garallex.stocks.domain

import com.garallex.stocks.TypeAliases.PriceSeries

object AveragePenetrationBuilder {
  def apply(price: PriceSeries, ema: Vector[EMA], length: Int): Vector[AveragePenetration] = {
    require(ema.nonEmpty, "AveragePenetrationBuilder: EMA must not be empty")
    require(price.nonEmpty, "AveragePenetrationBuilder: Price must not be empty")
    val firstDate = if (ema.head.date.isAfter(price.head.date)) ema.head.date else price.head.date
    val lastDate = if (ema.last.date.isBefore(price.last.date)) ema.last.date else price.last.date

    val priceSkipped = price.dropWhile(_.date.isBefore(firstDate)).takeWhile(p => p.date.isBefore(lastDate) || p.date.isEqual(lastDate))
    val emaSkipped = ema.dropWhile(_.date.isBefore(firstDate)).takeWhile(p => p.date.isBefore(lastDate) || p.date.isEqual(lastDate))

    require(emaSkipped.length == priceSkipped.length, "AveragePenetrationBuilder: EMA and Price after skipping dates must be same size")
    val zipped = emaSkipped.zip(priceSkipped)

    val result = collection.mutable.ArrayBuffer[AveragePenetration]()
    var sumPenetrated = BigDecimal(0)
    var cntPenetrated = 0

    for (i <- zipped.indices) {
      val (e, p) = zipped(i)
      require(e.date.equals(p.date), "AveragePenetrationBuilder: EMA and Price collections must have same dates")

      val penetration = (e.ema - p.low).max(0)
      if (penetration > 0) {
        cntPenetrated = cntPenetrated + 1
        sumPenetrated = sumPenetrated + penetration
      }

      val backIndex = (i - length) max 0

      if (backIndex > 0) {
        val (eBack, pBack) = zipped(backIndex - 1)
        val backPenetration = (eBack.ema - pBack.low).max(0)
        if (backPenetration > 0) {
          cntPenetrated = cntPenetrated - 1
          sumPenetrated = sumPenetrated - backPenetration
        }
      }

      result.append(AveragePenetration(e.date, if (cntPenetrated > 0) sumPenetrated / cntPenetrated else 0))
    }
    result.toVector
  }
}
