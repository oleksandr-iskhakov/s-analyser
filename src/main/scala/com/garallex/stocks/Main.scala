package com.garallex.stocks

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.garallex.stocks.datasource.PriceSource
import com.garallex.stocks.datasource.database.MongoStorage
import com.garallex.stocks.datasource.apisource._
import com.garallex.stocks.domain.{AveragePenetrationBuilder, ImpulseBuilder, Stock}
import com.garallex.stocks.technical.{SetupScanner, SetupScannerResult}
import com.garallex.stocks.technical.breakout.Breakout
import com.garallex.stocks.technical.impulse.ImpulseStrategy1
import org.json4s.JsonAST.JString
import org.json4s.{CustomSerializer, DefaultFormats}
import org.json4s.jackson.Serialization.write
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.{Completed, MongoClient, MongoClientSettings, MongoCollection, Observer}
import org.mongodb.scala.connection.ClusterSettings

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Try}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

object Main {
  def grahamPreFiltered(stock: Stock): Option[Boolean] =
    (stock.enterpriseValue,
      stock.currentRatio,
      stock.totalCurrentAssets,
      stock.longTermDebt,
      stock.peRatio,
      stock.priceToBook) match {
      case (Some(ev), Some(cr), Some(tca), Some(ltd), Some(per), Some(ptb)) => Some(
        ev >= 2000000000 &&
          cr >= 2 &&
          tca >= ltd &&
          per <= 15 &&
          ptb <= 1.5)
      case _ => None
    }

  private def fundamentalAnalyzer(): Unit = {
    while (true) {
      val ticker = scala.io.StdIn.readLine("Ticker:")
      if (ticker == null || ticker == "") return

      //      val stock1 = new StockBuilder(ticker.toUpperCase).build()
      //      val stock = if (ticker.toUpperCase() == "BABA") stock1.copy(beta = Some(BigDecimal("1.16"))) else stock1

      val stock = new StockBuilder(ticker.toUpperCase).build()

      println(new StockToString(stock))
    }
  }

  def main(args: Array[String]): Unit = {
    val ticker = "MSFT"

    val price = ApiPriceLoader.fetch(
      ticker = ticker,
      fetchType = FetchType.Compact)

    val dailyFastEma = ApiEMALoader.fetch(
      ticker = ticker,
      fetchType = FetchType.Compact,
      parameters = Map("interval" -> "daily", "time_period" -> "11", "series_type" -> "close"))

    val dailySlowEma = ApiEMALoader.fetch(
      ticker = ticker,
      fetchType = FetchType.Compact,
      parameters = Map("interval" -> "daily", "time_period" -> "22", "series_type" -> "close"))

    val weeklyFastEma = ApiEMALoader.fetch(
      ticker = ticker,
      fetchType = FetchType.Compact,
      parameters = Map("interval" -> "weekly", "time_period" -> "13", "series_type" -> "close"))

    val weeklySlowEma = ApiEMALoader.fetch(
      ticker = ticker,
      fetchType = FetchType.Compact,
      parameters = Map("interval" -> "weekly", "time_period" -> "26", "series_type" -> "close"))

    val dailyMacd = ApiMACDLoader.fetch(
      ticker = ticker,
      fetchType = FetchType.Compact,
      parameters = Map("interval" -> "daily", "series_type" -> "close"))

    val weeklyMacd = ApiMACDLoader.fetch(
      ticker = ticker,
      fetchType = FetchType.Compact,
      parameters = Map("interval" -> "weekly", "series_type" -> "close"))

    val dailyImpulse = ImpulseBuilder(dailyFastEma, dailyMacd)
    val weeklyImpulse = ImpulseBuilder(weeklyFastEma, weeklyMacd)

    val dailyFastEmaAvePenetration = AveragePenetrationBuilder(price, dailyFastEma, 30)
    val dailySlowEmaAvePenetration = AveragePenetrationBuilder(price, dailySlowEma, 30)

    val stochastic = ApiStochasticLoader.fetch(
      ticker = ticker,
      fetchType = FetchType.Compact,
      parameters = Map("interval" -> "daily"))

    val rsi = ApiRSILoader.fetch(
      ticker = ticker,
      fetchType = FetchType.Compact,
      parameters = Map("interval" -> "daily", "time_period" -> "14", "series_type" -> "close"))

    val atr = ApiATRLoader.fetch(
      ticker = ticker,
      fetchType = FetchType.Compact,
      parameters = Map("interval" -> "daily", "time_period" -> "66", "series_type" -> "close"))

    val strategy = new ImpulseStrategy1(price,
      weeklyFastEma,
      weeklySlowEma,
      dailyFastEma,
      dailySlowEma,
      atr,
      stochastic,
      dailyFastEmaAvePenetration,
      dailySlowEmaAvePenetration,
      rsi,
      weeklyMacd,
      dailyMacd)

    val result = strategy.backtest()

    println(result)
    //    fundamentalAnalyzer()
    //    val resultFsm = BreakoutFSM()
    //      .logAndReceive(BodyCut) // a
    //      .logAndReceive(ShadowCut) // b
    //      .logAndReceive(BodyTopLowerThanDelta) // d
    //      .logAndReceive(BodyTopLowerThanDelta) // d
    //      .logAndReceive(ShadowCut) //b
    //      .logAndReceive(BodyTopLowerThanDelta) // d
    //      .logAndReceive(BodyTopLowerThanDelta) // d
    //      //      .logAndReceive(CandleBodyCut) // a
    //      .logAndReceive(BodyTopLowerThanDelta) // d
    //      .logAndReceive(ShadowCut) //b
    //      .logAndReceive(BodyTopWithinDelta) //c
    //      .logAndReceive(BodyTopWithinDelta) //c
    //      .logAndReceive(BodyTopLowerThanDelta) // d
    //      .logAndReceive(BodyTopLowerThanDelta) // d
    //      .logAndReceive(BodyTopLowerThanDelta) // d
    //      .logAndReceive(ShadowCut) //b
    //
    //      .logAndReceive(BodyTopLowerThanDelta) // d
    //      .logAndReceive(BodyTopLowerThanDelta) // d
    //      .logAndReceive(BodyTopLowerThanDelta) // d
    //
    //    println(resultFsm)

    //    Try {
    //      val x = 1 / 0
    //    } match {
    //      case Failure(e) => println(e.getMessage + "\n\t" + e.getStackTrace.mkString("\n\t"))
    //    }

    val dbStorage = new MongoStorage
    val priceSource = new PriceSource(dbStorage)

    val lastExpectedDate = LocalDate.of(2017, 12, 21)

    val tickers = Source.fromFile("input.txt").getLines.filter(_.trim.length > 0).toList

    val scannerResult = new SetupScanner(
      portionSize = 20,
      priceSource = priceSource,
      lastExpectedDate = lastExpectedDate).scan(tickers)

    val matching = scannerResult.filter(_.result == Left(true))
    println(s"Scanner result: total = ${scannerResult.size}")
    println(s"Matching = ${matching.size}")
    matching.foreach(println)

    val errors = scannerResult.collect {
      case SetupScannerResult(ticker, _, Right(message)) => ticker -> message
    }

    println(s"Errors = ${errors.size}")
    errors.foreach(println)

    dbStorage.close()
  }
}
