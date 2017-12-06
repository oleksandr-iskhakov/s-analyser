package com.garallex.stocks.datasource.database

import java.time.LocalDate

import com.garallex.stocks.domain.Candle
import org.bson.{BsonReader, BsonWriter}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.types.Decimal128

class CandleCodec extends Codec[Candle] {
  override def encode(writer: BsonWriter, value: Candle, encoderContext: EncoderContext): Unit = {
    writer.writeDateTime(value.date.toEpochDay)
    //    writer.writeDecimal128(Decimal128.parse(value.open.toString()))
    //    writer.writeDecimal128(Decimal128.parse(value.high.toString()))
    //    writer.writeDecimal128(Decimal128.parse(value.low.toString()))
    //    writer.writeDecimal128(Decimal128.parse(value.close.toString()))
    //    writer.writeDecimal128(Decimal128.parse(value.volume.toString()))
  }

  override def getEncoderClass: Class[Candle] = classOf[Candle]

  override def decode(reader: BsonReader, decoderContext: DecoderContext): Candle =
    Candle(
      date = LocalDate.ofEpochDay(reader.readDateTime),
      open = BigDecimal(1), // BigDecimal(reader.readDecimal128("open").bigDecimalValue()),
      high = BigDecimal(1), //BigDecimal(reader.readDecimal128("high").bigDecimalValue()),
      low = BigDecimal(1), //BigDecimal(reader.readDecimal128("low").bigDecimalValue()),
      close = BigDecimal(1), //BigDecimal(reader.readDecimal128("close").bigDecimalValue()),
      volume = BigDecimal(1)) //BigDecimal(reader.readDecimal128("volume").bigDecimalValue()))
}
