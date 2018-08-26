package com.garallex.stocks.datasource

import java.time.LocalDate

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.datasource.apisource.{ApiPriceLoader, FetchType}
import com.garallex.stocks.technical.DbStorage
import com.typesafe.scalalogging.LazyLogging

import scala.async.Async.{async, await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PriceSource(dbStorage: DbStorage) extends LazyLogging {

  def load(ticker: String, lastExpectedDate: LocalDate): Future[PriceSeries] = async {
    await(dbStorage.fetchLastCandle(ticker)) match {
      case Some(lastCandleInCache) if lastCandleInCache.date.isBefore(lastExpectedDate) =>
        val apiPriceLoader = new ApiPriceLoader
        apiPriceLoader.fetch(ticker, FetchType.Compact)
        val price = apiPriceLoader.getResult
        val newestPrice = price.filter(_.date.isAfter(lastCandleInCache.date))
        await(dbStorage.insertCandles(ticker, newestPrice))
        logger.info(s"$ticker: cache partially updated with ${newestPrice.size} candles")
        await(dbStorage.fetchAllCandles(ticker))
      case Some(_) =>
        logger.info(s"$ticker: fetch from cache")
        await(dbStorage.fetchAllCandles(ticker))
      case None =>
        val apiPriceLoader = new ApiPriceLoader
        apiPriceLoader.fetch(ticker, FetchType.Full)
        val price = apiPriceLoader.getResult
        await(dbStorage.insertCandles(ticker, price))
        logger.info(s"$ticker: fetched full from API, inserted into cache ${price.size} candles")
        price.sortWith { case (c1, c2) => c2.date.isBefore(c1.date) }
    }
  }
}
