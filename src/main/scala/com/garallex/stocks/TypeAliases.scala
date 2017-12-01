package com.garallex.stocks

import java.time.LocalDate

import com.garallex.stocks.domain.Candle

object TypeAliases {
  type PriceSeries = List[(LocalDate, Candle)]
}
