package com.garallex.stocks.datasource

import java.io.Closeable
import java.time.LocalDate

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.datasource.apisource.{ApiPriceLoader, FetchType}
import com.garallex.stocks.datasource.database.MongoStorage

class PriceSource extends Closeable {
  private val mongoStorage = new MongoStorage()

  def load(ticker: String, lastExpectedDate: LocalDate): PriceSeries =
    mongoStorage.fetchLastCandle(ticker) match {
      case Some(lastCandleInCache) if lastCandleInCache.date.isBefore(lastExpectedDate) =>
        val price = ApiPriceLoader.fetch(ticker, FetchType.Compact)
        val newestPrice = price.filter(_.date.isAfter(lastCandleInCache.date))
        mongoStorage.insertCandles(ticker, newestPrice)
        mongoStorage.fetchAll(ticker)
      case Some(_) =>
        mongoStorage.fetchAll(ticker)
      case None =>
        val price = ApiPriceLoader.fetch(ticker, FetchType.Full)
        mongoStorage.insertCandles(ticker, price)
        price.sortWith { case (c1, c2) => c2.date.isBefore(c1.date) }
    }

  override def close(): Unit = mongoStorage.close()
}
