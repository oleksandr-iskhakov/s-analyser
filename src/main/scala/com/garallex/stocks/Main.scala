package com.garallex.stocks

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.garallex.stocks.datasource.PriceSource
import com.garallex.stocks.datasource.database.MongoStorage
import com.garallex.stocks.datasource.apisource.ApiPriceLoader
import com.garallex.stocks.domain.Stock
import com.garallex.stocks.technical.SetupScanner
import com.garallex.stocks.technical.breakout.Breakout
import org.json4s.JsonAST.JString
import org.json4s.{CustomSerializer, DefaultFormats}
import org.json4s.jackson.Serialization.write
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.{Completed, MongoClient, MongoClientSettings, MongoCollection, Observer}
import org.mongodb.scala.connection.ClusterSettings

import scala.concurrent.{Await, Future}
import scala.util.Try
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



    val lastExpectedDate = LocalDate.of(2017, 12, 8)

    val tickers = Source.fromFile("input.txt").getLines.filter(_.trim.length > 0).toList

//    val tickers = Iterator.continually(io.StdIn.readLine).takeWhile(_ != null).toList

//    val tickers = List("WYN", "MSFT", "GOOG", "CACC", "ASNA")
    //    val tickers = List("FRGI")


    val scannerResult = new SetupScanner().scan(tickers, lastExpectedDate)
    val matching = scannerResult.filter(_.result == Left(true))
    println(s"Scanner result: total = ${scannerResult.size}, matching = ${matching.size}:")
    scannerResult.foreach(println)

    //    val atr = BigDecimal(1.62)
    //    val result = new Breakout(price = price, deltaDown = atr / 2, deltaUp = atr / 2).screen()
    //    println(s"result: $result")
  }
}
