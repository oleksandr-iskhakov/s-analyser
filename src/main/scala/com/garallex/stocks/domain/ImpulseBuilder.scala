package com.garallex.stocks.domain

import java.time.LocalDate

import com.garallex.stocks.domain.ImpulseDirection.ImpulseDirection

case class Impulse(date: LocalDate, impulse: ImpulseDirection)

object ImpulseDirection extends Enumeration {
  type ImpulseDirection = Value
  val Bullish, Bearish, Neutral = Value
}

object ImpulseBuilder {
  def apply(ema: Vector[EMA], macd: Vector[MACD]): Vector[Impulse] = {
    require(ema.nonEmpty, "ImpulseBuilder: EMA must not be empty")
    require(macd.nonEmpty, "ImpulseBuilder: MACD must not be empty")
    val firstDate = if (ema.head.date.isAfter(macd.head.date)) ema.head.date else macd.head.date

    val macdSkipped = macd.dropWhile(_.date.isBefore(firstDate))
    val emaSkipped = ema.dropWhile(_.date.isBefore(firstDate))

    require(emaSkipped.length == macdSkipped.length, "ImpulseBuilder: EMA and MACD after skipping dates must be same size")
    val zipped = emaSkipped.zip(macdSkipped)

    val result = collection.mutable.ArrayBuffer[Impulse]()

    for (i <- zipped.indices) {
      val (e, m) = zipped(i)
      require(e.date.equals(m.date), "ImpulseBuilder: EMA and MACD collections must have same dates")
      if (i == 0)
        result.append(Impulse(e.date, ImpulseDirection.Neutral))
      else {
        val (ePrev, mPrev) = zipped(i - 1)
        val direction =
          if (e.ema > ePrev.ema && m.macdHist > mPrev.macdHist)
            ImpulseDirection.Bullish
          else if (e.ema < ePrev.ema && m.macdHist < mPrev.macdHist)
            ImpulseDirection.Bearish
          else
            ImpulseDirection.Neutral
        result.append(Impulse(e.date, direction))
      }
    }
    result.toVector
  }
}
