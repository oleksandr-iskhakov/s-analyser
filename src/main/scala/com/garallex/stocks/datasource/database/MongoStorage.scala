package com.garallex.stocks.datasource.database

import java.time.LocalDate

import com.garallex.stocks.TypeAliases.PriceSeries
import com.garallex.stocks.domain.Candle
import com.garallex.stocks.technical.DbStorage
import org.bson.codecs.configuration.CodecRegistries
import org.bson.codecs.configuration.CodecRegistries.fromRegistries
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{IndexOptions, Indexes}
import org.mongodb.scala.{Completed, MongoClient, MongoCollection}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MongoStorage extends DbStorage {
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


  private def createIndexIfNotExist(mongoCollection: MongoCollection[Document]): Future[String] =
    mongoCollection
      .listIndexes()
      .toFuture()
      .flatMap { indexes =>
        if (!indexes.exists(_.keys.exists(_ == "IDX_DATE")))
          mongoCollection.createIndex(
            Indexes.descending("date"),
            IndexOptions().name("IDX_DATE").background(true).unique(true))
            .toFuture()
        else Future {
          ""
        }
      }

  private def getCollection(ticker: String) = mongoDb.getCollection(ticker)

  def insertCandles(ticker: String, price: PriceSeries): Future[Completed] =
    createIndexIfNotExist(getCollection(ticker))
      .flatMap { _ =>
        getCollection(ticker)
          .insertMany(price.map(writer))
          .toFuture()
      }

  def fetchLastCandle(ticker: String): Future[Option[Candle]] =
    mongoDb
      .getCollection(ticker)
      .find()
      .sort(Document("{ date: -1 }"))
      .limit(1)
      .toFuture()
      .map {
        case Nil => None
        case head :: Nil => Some(reader(head))
      }

  def fetchAllCandles(ticker: String): Future[PriceSeries] =
    mongoDb
      .getCollection(ticker)
      .find()
      .sort(Document("{ date: -1 }"))
      .toFuture()
      .map(_.map(reader).toVector)

  override def close(): Unit = mongoClient.close()
}
