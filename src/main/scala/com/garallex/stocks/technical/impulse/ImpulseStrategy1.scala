package com.garallex.stocks.technical.impulse

import java.time.LocalDate

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.domain._
import com.garallex.stocks.domain.Orderings._
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

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
                       dailyMacd: Vector[MACD]) {

  private lazy val logger: Logger =
    Logger(LoggerFactory.getLogger("Channel"))

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

  private def lookup[T <: DateBased](src: Vector[T], date: LocalDate, ndx: Int = 0): T = src(lookupForIndex(src, date, ndx))

  @tailrec
  private def lookupForIndex[T <: DateBased](src: Vector[T], date: LocalDate, ndx: Int = 0): Int = {
    val result = src(ndx)
    if ((result.date.isAfter(date) || result.date.isEqual(date)) && (ndx == src.length - 1 || date.isBefore(src(ndx + 1).date)))
      ndx
    else
      lookupForIndex(src, date, ndx + 1)
  }

  private def buyCondition(currentPrice: Candle): Boolean = {
    val currentImpulseWeekly = lookupWeekly(impulseWeekly, currentPrice.date)
    val currentImpulseDaily = lookup(impulseDaily, currentPrice.date)
    val currentStochastic = lookup(stochastic, currentPrice.date)
    val currentRsi = lookup(rsi, currentPrice.date)
    val currentFastEma = lookup(dailyFastEma, currentPrice.date)

    val currentDailyNdx = lookupForIndex(dailySlowEma, currentPrice.date)
    val currentSlowEma = dailySlowEma(currentDailyNdx)

    require(currentPrice.date.isEqual(currentImpulseDaily.date))
    require(currentPrice.date.isEqual(currentStochastic.date))
    require(currentPrice.date.isEqual(currentRsi.date))
    require(currentPrice.date.isEqual(currentFastEma.date))
    require(currentPrice.date.isEqual(currentSlowEma.date))

    val impulseCondition =
      acceptedImpulses.contains(currentImpulseWeekly.impulse) &&
        acceptedImpulses.contains(currentImpulseDaily.impulse)

    val oscillatorsOversold = currentStochastic.slowK < 30 || currentRsi.rsi < 30

    val uptrend = dailySlowEma(currentDailyNdx).ema > dailySlowEma(currentDailyNdx - 1).ema &&
      dailyFastEma(currentDailyNdx).ema > dailySlowEma(currentDailyNdx).ema

    val priceInValueZone = true //!(currentPrice.low > currentFastEma.ema || currentPrice.high < currentSlowEma.ema)

    impulseCondition && oscillatorsOversold && uptrend && priceInValueZone
  }

  private def formEntryAndStopLossOrders(currentPrice: Candle): (Order, Order) = {
    val currentSlowEmaNdx = lookupForIndex(dailySlowEma, currentPrice.date)
    val currentFastEmaNdx = lookupForIndex(dailyFastEma, currentPrice.date)
    val projectedFastEma = dailyFastEma(currentFastEmaNdx).ema + (dailyFastEma(currentFastEmaNdx).ema - dailyFastEma(currentFastEmaNdx - 1).ema)
    val entryPrice = projectedFastEma - fastEmaAveragePenetration(currentFastEmaNdx).penetration
    val entryOrder = Order(entryPrice, 100)
    val slOrder = Order(dailySlowEma(currentSlowEmaNdx).ema - 2 * slowEmaAveragePenetration(currentSlowEmaNdx).penetration, 100)
    (entryOrder, slOrder)
  }

  private def minAvailableDate(input: Seq[Vector[DateBased]]) = input.map(_.head.date).max


  def backtest(startDate: Option[LocalDate] = None): Vector[Position] = {
    val closedPositions = collection.mutable.ArrayBuffer[Position]()
    var openedPosition: Option[Position] = None
    var entryOrder: Option[Order] = None
    var slOrder: Option[Order] = None

    val minAvailDate = minAvailableDate(Seq(price,
      dailyFastEma,
      dailySlowEma,
      atr,
      stochastic,
      fastEmaAveragePenetration,
      slowEmaAveragePenetration,
      rsi,
      dailyMacd))

    val minDate = startDate match {
      case Some(date) if date.isAfter(minAvailDate) => date
      case _ => minAvailDate
    }

    for (currentPrice <- price.dropWhile(_.date.isBefore(minDate)).drop(1)) {

      val lastSlowEma = lookup(dailySlowEma, currentPrice.date)
      val lastFastEma = lookup(dailyFastEma, currentPrice.date)
      val lastAtr = lookup(atr, currentPrice.date)

      require(currentPrice.date.isEqual(lastSlowEma.date), s"Req 1 failed. Current Price: ${currentPrice.date}, Slow EMA: ${lastSlowEma.date}")
      require(currentPrice.date.isEqual(lastFastEma.date), s"Req 2 failed. Current Price: ${currentPrice.date}, Fast EMA: ${lastFastEma.date}")
      require(currentPrice.date.isEqual(lastAtr.date), s"Req 3 failed. Current Price: ${currentPrice.date}, ATR: ${lastAtr.date}")

      val currentImpulseWeekly = lookupWeekly(impulseWeekly, currentPrice.date)
      val currentImpulseDaily = lookup(impulseDaily, currentPrice.date)
      logger.info("Price: " + currentPrice + "Weekly: " + currentImpulseWeekly + " Daily: " + currentImpulseDaily)

      (entryOrder, openedPosition) match {

        // No Position yet. Check if we can open one with an Order placed earlier
        case (Some(order), None) if currentPrice.low <= order.stopPrice && order.stopPrice <= currentPrice.high =>
          val r = (entryOrder.get.stopPrice - slOrder.get.stopPrice).abs
          openedPosition = Some(Position(currentPrice.date, order.stopPrice, Direction.Long, 100, r))
          logger.info("OPENED: " + openedPosition)
          entryOrder = None

        // No Position yet. We couldn't open a position with Order placed earlier
        case (Some(_), None) =>
          if (buyCondition(currentPrice)) {
            val (newEntryOrder, newSlOrder) = formEntryAndStopLossOrders(currentPrice)
            entryOrder = Some(newEntryOrder)
            slOrder = Some(newSlOrder)
            logger.info("ENTRY ORDER: " + newEntryOrder)
            logger.info("SL ORDER: " + newSlOrder)
          } else {
            entryOrder = None
            slOrder = None
          }

        // No Position yet. Place order if Entry condition met
        case (None, None) =>
          if (buyCondition(currentPrice)) {
            val (newEntryOrder, newSlOrder) = formEntryAndStopLossOrders(currentPrice)
            entryOrder = Some(newEntryOrder)
            slOrder = Some(newSlOrder)
            logger.info("ENTRY ORDER: " + newEntryOrder)
            logger.info("SL ORDER: " + newSlOrder)
          }

        // Check closing conditions
        case (None, Some(position))
          if currentPrice.low <= lastSlowEma.ema + 2 * lastAtr.atr && lastSlowEma.ema + 2 * lastAtr.atr <= currentPrice.high =>

          val closedPosition = position.copy(
            exitDate = Some(currentPrice.date),
            exitPrice = Some(lastSlowEma.ema + 2 * lastAtr.atr),
            exitReason = Some(ExitReason.TakeProfit))
          closedPositions.append(closedPosition)
          openedPosition = None

        case (None, Some(position))
          if currentPrice.low <= slOrder.get.stopPrice =>
          logger.info("SL FIRED")
          logger.info(s"Current Price: $currentPrice, SL: $slOrder")

          val closedPosition = position.copy(
            exitDate = Some(currentPrice.date),
            exitPrice = Some(slOrder.get.stopPrice),
            exitReason = Some(ExitReason.StopLoss))
          closedPositions.append(closedPosition)
          openedPosition = None

        // Move SL each time price moves 0.66 R towards my direction
        case (None, Some(position))
          if currentPrice.close > slOrder.get.stopPrice + 0.66 * position.r =>
          slOrder = slOrder.map(_.copy(stopPrice = slOrder.get.stopPrice + 0.66 * position.r))
          logger.info("SL MOVED")
          logger.info(s"Current Price: $currentPrice, SL: $slOrder")

        case (_, Some(position)) if currentPrice == price.last =>
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
