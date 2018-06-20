package com.garallex.stocks.technical.impulse

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.domain._

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
                       macdWeekly: Vector[MACD],
                       macdDaily: Vector[MACD]) {

  private val impulseWeekly = ImpulseBuilder(weeklyFastEma, macdWeekly)
  private val impulseDaily = ImpulseBuilder(dailyFastEma, macdDaily)

  private final val acceptedImpulses = List(ImpulseDirection.Bullish, ImpulseDirection.Neutral)

  private def buyCondition(currentNdx: Int): Boolean = {
    val currentPrice = price(currentNdx)
    val currentImpulseWeekly = impulseWeekly(currentNdx).impulse
    val currentImpulseDaily = impulseDaily(currentNdx).impulse

    val impulseCondition =
      acceptedImpulses.contains(currentImpulseWeekly) &&
        acceptedImpulses.contains(currentImpulseDaily)

    val oscillatorsOversold = stochastic(currentNdx).slowK < 20 || rsi(currentNdx).rsi < 30

    val uptrend =
      dailySlowEma(currentNdx).ema > dailySlowEma(currentNdx - 1).ema &&
        dailyFastEma(currentNdx).ema > dailySlowEma(currentNdx).ema

    val priceInValueZone = !(currentPrice.low > dailyFastEma(currentNdx).ema || currentPrice.high < dailySlowEma(currentNdx).ema)

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

    for (currentNdx <- price.indices.drop(1)) {
      val currentPrice = price(currentNdx)
      val currentSlowEma = dailySlowEma(currentNdx)
      val currentFastEma = dailyFastEma(currentNdx)
      val currentAtr = atr(currentNdx)

      (entryOrder, openedPosition) match {
        case (Some(order), None) if currentPrice.low >= order.stopPrice && order.stopPrice <= currentPrice.high =>
          openedPosition = Some(Position(currentPrice.date, entryPrice = order.stopPrice, Direction.Long, 100))
          entryOrder = None

        case (Some(_), None) =>
          if (buyCondition(currentNdx)) {
            val (newEntryOrder, newSlOrder) = getEntryOrders(currentNdx)
            entryOrder = Some(newEntryOrder)
            slOrder = Some(newSlOrder)
          } else {
            entryOrder = None
            slOrder = None
          }

        case (None, None) =>
          if (buyCondition(currentNdx)) {
            val (newEntryOrder, newSlOrder) = getEntryOrders(currentNdx)
            entryOrder = Some(newEntryOrder)
            slOrder = Some(newSlOrder)
          }

        case (None, Some(position))
          if currentPrice.low <= currentSlowEma.ema + 2 * currentAtr.atr && currentPrice.high >= currentSlowEma.ema + 2 * currentAtr.atr =>

          val closedPosition = position.copy(
            exitDate = Some(currentPrice.date),
            exitPrice = Some(currentSlowEma.ema + 2 * currentAtr.atr),
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

        case (_, Some(position)) if currentNdx == price.indices.last =>
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
