package com.garallex.stocks.datasource

import java.io.Closeable
import java.time.LocalDate

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.datasource.apisource.{ApiPriceLoader, FetchType}
import com.garallex.stocks.datasource.database.MongoStorage
import com.typesafe.scalalogging.LazyLogging

class PriceSource extends Closeable with LazyLogging {
  private val mongoStorage = new MongoStorage

  def load(ticker: String, lastExpectedDate: LocalDate): PriceSeries =
    mongoStorage.fetchLastCandle(ticker) match {
      case Some(lastCandleInCache) if lastCandleInCache.date.isBefore(lastExpectedDate) =>
        val price = ApiPriceLoader.fetch(ticker, FetchType.Compact)
        val newestPrice = price.filter(_.date.isAfter(lastCandleInCache.date))
        mongoStorage.insertCandles(ticker, newestPrice)
        logger.info(s"$ticker: cache partially updated with ${newestPrice.size} candles")
        mongoStorage.fetchAll(ticker)
      case Some(_) =>
        logger.info(s"$ticker: fetch from cache")
        mongoStorage.fetchAll(ticker)
      case None =>
        val price = ApiPriceLoader.fetch(ticker, FetchType.Full)
        mongoStorage.insertCandles(ticker, price)
        logger.info(s"$ticker: fetched full from API, inserted into cache ${price.size} candles")
        price.sortWith { case (c1, c2) => c2.date.isBefore(c1.date) }
    }

  override def close(): Unit = mongoStorage.close()
}
