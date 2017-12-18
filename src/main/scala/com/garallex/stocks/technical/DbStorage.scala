package com.garallex.stocks.technical

import java.io.Closeable

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.domain.Candle
import org.mongodb.scala.Completed

import scala.concurrent.Future

trait DbStorage extends Closeable {
  def fetchLastCandle(ticker: String): Future[Option[Candle]]
  def fetchAllCandles(ticker: String): Future[PriceSeries]
  def insertCandles(ticker: String, price: PriceSeries): Future[Completed]
}
