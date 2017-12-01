package com.garallex.stocks

import com.garallex.stocks.domain.Stock
import com.garallex.stocks.technical.screeners.Breakout


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

    val price = PriceLoader.load("WYN")
    val atr = BigDecimal(1.62)
    val result = new Breakout(price = price, deltaDown = atr / 2, deltaUp = atr / 2).screen()
    println(s"result: $result")
  }
}
