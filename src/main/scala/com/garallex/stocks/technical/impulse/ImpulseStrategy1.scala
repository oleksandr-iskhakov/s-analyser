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
  def backtest(): Vector[Position] = {
    val impulseWeekly = ImpulseBuilder(weeklyFastEma, macdWeekly)
    val impulseDaily = ImpulseBuilder(dailyFastEma, macdDaily)
    val closedPositions = collection.mutable.ArrayBuffer[Position]()
    var openedPosition: Option[Position] = None
    var entryOrder: Option[Order] = None
    var slOrder: Option[Order] = None

    for (currentNdx <- price.indices) {
      val currentPrice = price(currentNdx)
      val currentSlowEma = dailySlowEma(currentNdx)
      val currentAtr = atr(currentNdx)

      (entryOrder, openedPosition) match {
        case (Some(order), None) if currentPrice.low >= order.stopPrice && order.stopPrice <= currentPrice.high =>
          openedPosition = Some(Position(currentPrice.date, entryPrice = order.stopPrice, 100))
        case (Some(_), None) =>
          val impulseCondition =
            (impulseWeekly(currentNdx).impulse == ImpulseDirection.Bullish || impulseWeekly(currentNdx).impulse == ImpulseDirection.Neutral) &&
              (impulseDaily(currentNdx).impulse == ImpulseDirection.Bullish || impulseDaily(currentNdx).impulse == ImpulseDirection.Neutral)

          val oscillatorsOversold = stochastic(currentNdx).slowK < 20 || rsi(currentNdx).rsi < 30

          val uptrend =
            dailySlowEma(currentNdx).ema > dailySlowEma(currentNdx - 1).ema &&
              dailyFastEma(currentNdx).ema > dailySlowEma(currentNdx).ema

          val priceInValueZone = !(currentPrice.low > dailyFastEma(currentNdx).ema || currentPrice.high < dailySlowEma(currentNdx).ema)

          val buyCondition = impulseCondition && oscillatorsOversold && uptrend && priceInValueZone

          if (buyCondition) {
            entryOrder = Some(Order(dailyFastEma(currentNdx).ema - fastEmaAveragePenetration(currentNdx).penetration, 100))
            slOrder = Some(Order(dailySlowEma(currentNdx).ema - 2 * slowEmaAveragePenetration(currentNdx).penetration, 100))
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

      }
    }
    closedPositions.toVector
  }
}
