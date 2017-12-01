package com.garallex.stocks.technical.screeners

import java.time.LocalDate

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.domain.{Candle, PriceRange}
import com.garallex.stocks.technical.screeners.BreakoutFSM._

import scala.annotation.tailrec

class Breakout(price: PriceSeries) {
  private def isAbove(candle: Candle, priceRange: PriceRange): Boolean =
    candle.bottomOfCandleBody >= priceRange.level

  private def isBelow(candle: Candle, priceRange: PriceRange): Boolean =
    candle.high < priceRange.from

  private def isBodyCut(candle: Candle, priceRange: PriceRange): Boolean =
    candle.bottomOfCandleBody < priceRange.level && candle.topOfCandleBody >= priceRange.level

  private def isShadowCut(candle: Candle, priceRange: PriceRange): Boolean =
    !(candle.topOfCandleBody > priceRange.to || candle.high < priceRange.from)

  @tailrec
  private def screenRec(fsm: BreakoutFSM, price: PriceSeries, targetRange: PriceRange): Boolean =
    if (fsm.state == Fail || price.isEmpty)
      false
    else if (fsm.state == Match)
      true
    else if (isAbove(price.head._2, targetRange))
      screenRec(fsm.logAndReceive(Above), price.tail, targetRange)
    else if (isBelow(price.head._2, targetRange))
      screenRec(fsm.logAndReceive(Below), price.tail, targetRange)
    else if (isBodyCut(price.head._2, targetRange))
      screenRec(fsm.logAndReceive(BodyCut), price.tail, targetRange)
    else if (isShadowCut(price.head._2, targetRange))
      screenRec(fsm.logAndReceive(ShadowCut), price.tail, targetRange)
    else
      throw new Exception()

  def screen(): Boolean = {
    val priceToScreen = price.reverse
    val lastClose = priceToScreen.head._2.bottomOfCandleBody
    val targetRange = PriceRange(lastClose, lastClose - BigDecimal(0.1), lastClose + BigDecimal(0.1))

    screenRec(BreakoutFSM(), priceToScreen.tail, targetRange)
  }
}
