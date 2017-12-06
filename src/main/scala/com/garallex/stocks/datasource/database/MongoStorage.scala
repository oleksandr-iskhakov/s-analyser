package com.garallex.stocks.datasource.database

import java.io.Closeable
import java.time.LocalDate

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.domain.Candle
import org.bson.codecs.configuration.CodecRegistries
import org.bson.codecs.configuration.CodecRegistries.fromRegistries
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{IndexOptions, Indexes}
import org.mongodb.scala.{Completed, MongoClient, MongoCollection, Observer}

import scala.concurrent.Await
import scala.concurrent.duration._


class MongoStorage extends Closeable {
  private val codecRegistry = fromRegistries(CodecRegistries.fromCodecs(new CandleCodec), DEFAULT_CODEC_REGISTRY)

  private val mongoClient = MongoClient()
  private val mongoDb = mongoClient.getDatabase("PriceSeries")

  private def writer(o: Candle): Document =
    Document(
      "date" -> BsonString(o.date.toString),
      "open" -> o.open,
      "high" -> o.high,
      "low" -> o.low,
      "close" -> o.close,
      "volume" -> o.volume)

  private def reader(doc: Document) =
    Candle(
      date = LocalDate.parse(doc.get("date").get.asString().getValue),
      open = BigDecimal(doc.get("open").get.asDecimal128().getValue.toString),
      high = BigDecimal(doc.get("high").get.asDecimal128().getValue.toString),
      low = BigDecimal(doc.get("low").get.asDecimal128().getValue.toString),
      close = BigDecimal(doc.get("close").get.asDecimal128().getValue.toString),
      volume = BigDecimal(doc.get("volume").get.asDecimal128().getValue.toString))


  private def createIndexIfNotExist(mongoCollection: MongoCollection[Document]): Unit = {
    val indexes = Await.result(mongoCollection
      .listIndexes()
      .toFuture(), 10 seconds)

    if (!indexes.exists(_.keys.exists(_ == "IDX_DATE")))
      Await.result(mongoCollection.createIndex(
        Indexes.descending("date"),
        IndexOptions().name("IDX_DATE").background(true).unique(true))
        .toFuture(), 10 seconds)
  }

  private def getCollection(ticker: String) = mongoDb.getCollection(ticker)

  def insertCandles(ticker: String, price: PriceSeries): Unit = {
    createIndexIfNotExist(mongoDb.getCollection(ticker))
    val mongoCollection = getCollection(ticker)

    Await.result(mongoCollection
      .insertMany(price.map(writer))
      .toFuture(), 10 seconds)
  }

  def insertCandle(ticker: String, candle: Candle): Unit = {
    val mongoCollection = getCollection(ticker)

    val o = mongoCollection.insertOne(writer(candle))

    o.subscribe(new Observer[Completed] {
      override def onNext(result: Completed): Unit = println("MongoLogger.onNext() called")

      override def onError(e: Throwable): Unit = println(s"MongoLogger.onError() called", e)

      override def onComplete(): Unit = println("MongoLogger.onComplete() called")
    })
  }


  def fetchLastCandle(ticker: String): Option[Candle] = {
    val f = mongoDb
      .getCollection(ticker)
      .find()
      .sort(Document("{ date: -1 }"))
      .limit(1)
      .toFuture()

    Await.result(f, 5 seconds) match {
      case Nil => None
      case head :: Nil => Some(reader(head))
    }
  }

  def fetchAll(ticker: String): PriceSeries = {
    val f = mongoDb
      .getCollection(ticker)
      .find()
      .sort(Document("{ date: -1 }"))
      .toFuture()
    Await.result(f, 5 seconds)
      .map(reader)
      .toList
  }

  override def close(): Unit = mongoClient.close()
}
