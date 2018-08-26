package com.garallex.stocks.technical.impulse

import java.time.LocalDate

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.domain._
import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec

class ImpulseStrategy1(price: PriceSeries,
                       weeklyFastEma: Vector[EMA],
                       weeklySlowEma: Vector[EMA],
                       dailyFastEma: Vector[EMA],
                       dailySlowEma: Vector[EMA],
                       atr: Vector[ATR],
                       stochastic: Vector[Stochastic],
                       fastEmaAveragePenetration: Vector[AveragePenetration],
                       slowEmaAveragePenetration: Vector[AveragePenetration],
                       rsi: Vector[RSI],
                       weeklyMacd: Vector[MACD],
                       dailyMacd: Vector[MACD]) extends LazyLogging {

  private val impulseWeekly = ImpulseBuilder(weeklyFastEma, weeklyMacd) // CAUTION: Date here is the LAST day of the week!
  private val impulseDaily = ImpulseBuilder(dailyFastEma, dailyMacd)

  private final val acceptedImpulses = List(ImpulseDirection.Bullish, ImpulseDirection.Neutral)

  @tailrec
  private def lookupWeekly[T <: DateBased](src: Vector[T], date: LocalDate, ndx: Int = 1): T = {
    val result = src(ndx - 1)
    if (ndx == src.indices.last || (result.date.isAfter(date) || result.date.isEqual(date)) && date.isBefore(src(ndx).date))
      result
    else
      lookupWeekly(src, date, ndx + 1)
  }

  /*
    Lookups for the candle right BEFORE the date specified
   */
  @tailrec
  private def lookup[T <: DateBased](src: Vector[T], date: LocalDate, ndx: Int = 0): T = {
    val result = src(ndx)
    if ((result.date.isAfter(date) || result.date.isEqual(date)) && (ndx == src.length - 1 || date.isBefore(src(ndx + 1).date)))
      result
    else
      lookup(src, date, ndx + 1)
  }

  private def buyCondition(currentNdx: Int): Boolean = {
    val currentPrice = price(currentNdx)

    val currentImpulseWeekly = lookupWeekly(impulseWeekly, currentPrice.date)
    val currentImpulseDaily = lookup(impulseDaily, currentPrice.date)
    val currentStochastic = lookup(stochastic, currentPrice.date)
    val currentRsi = lookup(rsi, currentPrice.date)
    val currentFastEma = lookup(dailyFastEma, currentPrice.date)
    val currentSlowEma = lookup(dailySlowEma, currentPrice.date)

    require(currentPrice.date.isEqual(currentImpulseDaily.date))
    require(currentPrice.date.isEqual(currentStochastic.date))
    require(currentPrice.date.isEqual(currentRsi.date))
    require(currentPrice.date.isEqual(currentFastEma.date))
    require(currentPrice.date.isEqual(currentSlowEma.date))


    val impulseCondition =
      acceptedImpulses.contains(currentImpulseWeekly.impulse) &&
        acceptedImpulses.contains(currentImpulseDaily.impulse)

    val oscillatorsOversold = currentStochastic.slowK < 20 || currentRsi.rsi < 30

    val uptrend = true
    //      dailySlowEma(currentNdx).ema > dailySlowEma(currentNdx - 1).ema &&
    //        dailyFastEma(currentNdx).ema > dailySlowEma(currentNdx).ema

    val priceInValueZone = true //!(currentPrice.low > currentFastEma.ema || currentPrice.high < currentSlowEma.ema)

    impulseCondition && oscillatorsOversold && uptrend && priceInValueZone
  }

  private def getEntryOrders(currentNdx: Int): (Order, Order) = {
    val currentSlowEma = dailySlowEma(currentNdx)
    val currentFastEma = dailyFastEma(currentNdx)
    val projectedFastEMA = currentFastEma.ema + (currentFastEma.ema - dailyFastEma(currentNdx - 1).ema)
    val entryPrice = projectedFastEMA - fastEmaAveragePenetration(currentNdx).penetration
    val entryOrder = Order(entryPrice, 100)
    val slOrder = Order(currentSlowEma.ema - 2 * slowEmaAveragePenetration(currentNdx).penetration, 100)
    (entryOrder, slOrder)
  }

  def backtest(): Vector[Position] = {
    val closedPositions = collection.mutable.ArrayBuffer[Position]()
    var openedPosition: Option[Position] = None
    var entryOrder: Option[Order] = None
    var slOrder: Option[Order] = None

    for (ndx <- price.indices.drop(1)) {
      val currentPrice = price(ndx)

      val lastSlowEma = lookup(dailySlowEma, currentPrice.date)
      val lastFastEma = lookup(dailyFastEma, currentPrice.date)
      val lastAtr = lookup(atr, currentPrice.date)

      require(currentPrice.date.isEqual(lastSlowEma.date), "req 1 failed")
      require(currentPrice.date.isEqual(lastFastEma.date), "req 2 failed")
      require(currentPrice.date.isEqual(lastAtr.date), "req 3 failed")

      val currentImpulseWeekly = lookupWeekly(impulseWeekly, currentPrice.date)
      val currentImpulseDaily = lookup(impulseDaily, currentPrice.date)
      logger.info("Price: " + currentPrice + "Weekly: " + currentImpulseWeekly + " Daily: " + currentImpulseDaily)

      if (buyCondition(ndx)) println("Buy on " + currentPrice.date)

      (entryOrder, openedPosition) match {
        case (Some(order), None) if currentPrice.low >= order.stopPrice && order.stopPrice <= currentPrice.high =>
          openedPosition = Some(Position(currentPrice.date, entryPrice = order.stopPrice, Direction.Long, 100))
          entryOrder = None

        case (Some(_), None) =>
          if (buyCondition(ndx)) {
            val (newEntryOrder, newSlOrder) = getEntryOrders(ndx)
            entryOrder = Some(newEntryOrder)
            slOrder = Some(newSlOrder)
          } else {
            entryOrder = None
            slOrder = None
          }

        case (None, None) =>
          if (buyCondition(ndx)) {
            val (newEntryOrder, newSlOrder) = getEntryOrders(ndx)
            entryOrder = Some(newEntryOrder)
            slOrder = Some(newSlOrder)
          }

        case (None, Some(position))
          if currentPrice.low <= lastSlowEma.ema + 2 * lastAtr.atr && currentPrice.high >= lastSlowEma.ema + 2 * lastAtr.atr =>

          val closedPosition = position.copy(
            exitDate = Some(currentPrice.date),
            exitPrice = Some(lastSlowEma.ema + 2 * lastAtr.atr),
            exitReason = Some(ExitReason.TakeProfit))
          closedPositions.append(closedPosition)
          openedPosition = None

        case (None, Some(position))
          if currentPrice.low <= slOrder.get.stopPrice && currentPrice.high >= slOrder.get.stopPrice =>

          val closedPosition = position.copy(
            exitDate = Some(currentPrice.date),
            exitPrice = Some(slOrder.get.stopPrice),
            exitReason = Some(ExitReason.StopLoss))
          closedPositions.append(closedPosition)
          openedPosition = None

        case (_, Some(position)) if ndx == price.indices.last =>
          val closedPosition = position.copy(
            exitDate = Some(currentPrice.date),
            exitPrice = Some(currentPrice.close),
            exitReason = Some(ExitReason.LastCandle))
          closedPositions.append(closedPosition)
          openedPosition = None

        case (None, Some(_)) =>
      }
    }
    closedPositions.toVector
  }
}
