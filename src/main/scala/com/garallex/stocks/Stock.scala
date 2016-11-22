package com.garallex.stocks

import com.garallex.stocks.Utils._

case class Stock(ticker: String,
                 name: String,
                 industry: String,
                 cashFlow: Option[BigDecimal],
                 longTermGrowth: Option[BigDecimal],
                 beta: Option[BigDecimal],
                 sharesOutstanding: Option[BigDecimal],
                 debtToEquity: Option[BigDecimal],
                 roe: Option[BigDecimal],
                 actualPrice: Option[BigDecimal],
                 peRatio: Option[BigDecimal]) {

  val intrinsicValue = (cashFlow, longTermGrowth, beta, sharesOutstanding) match {
    case (Some(cf), Some(ltg), Some(b), Some(so)) => Some(calcIntrinsicValue(cf, ltg, b, so))
    case _ => None
  }

  private def calcIntrinsicValue(cashFlow: BigDecimal,
                         longTermGrowthRate: BigDecimal,
                         beta: BigDecimal,
                         sharesOutstanding: BigDecimal): BigDecimal = {

    def calcDiscountRateByBeta(beta: BigDecimal): BigDecimal =
      (if (beta < 0.8) BigDecimal("5")
      else if (beta < 1) BigDecimal("5.8") // ???
      else if (beta < 1.1) BigDecimal("6")
      else if (beta < 1.2) BigDecimal("6.8")
      else if (beta < 1.3) BigDecimal("7")
      else if (beta < 1.4) BigDecimal("7.9")
      else if (beta < 1.5) BigDecimal("8")
      else if (beta < 1.6) BigDecimal("8.9")
      else BigDecimal("9")) / BigDecimal("100")

    val p = new collection.mutable.ArrayBuffer[BigDecimal]
    p.append(cashFlow * (1 + longTermGrowthRate))
    for (i <- 1 until 10) p.append(p(i - 1) * (1 + longTermGrowthRate))
    val discountRate = calcDiscountRateByBeta(beta)
    for (i <- p.indices) p.insert(i, p(i) / math.pow((1 + discountRate).toDouble, i + 1))
    p.sum / sharesOutstanding
  }

  def isComplete =
    debtToEquity.isDefined &&
      roe.isDefined &&
      peRatio.isDefined &&
      intrinsicValue.isDefined &&
      actualPrice.isDefined &&
      cashFlow.isDefined &&
      longTermGrowth.isDefined &&
      beta.isDefined &&
      sharesOutstanding.isDefined

  def actualValueToIntrinsicValuePercent() =
    (actualPrice, intrinsicValue) match {
      case (_, None) | (None, _) => None
      case (Some(actualPriceValue), Some(intrinsicValueValue)) => Some(100 * (actualPriceValue / intrinsicValueValue - 1))
    }

  private def decimalOptionToString(value: Option[BigDecimal],
                                    multiplier: BigDecimal = 1,
                                    format: String = "") = value match {
    case Some(v) if format == "" => (v * multiplier).toString()
    case Some(v) => (v * multiplier).formatted(format)
    case _ => ""
  }

  override def toString =
    new StringBuilder()
      .append(s"$ticker - $name\n")
      .append(s"Debt to equity, %                   " + decimalOptionToString(debtToEquity, 100) + "\n")
      .append(s"ROE, %                              " + decimalOptionToString(roe, 100) + "\n")
      .append(s"P/E                                 " + decimalOptionToString(peRatio) + "\n")
      .append(s"Actual price                        " + decimalOptionToString(actualPrice) + "\n")
      .append(s"Intrinsic value                     " + decimalOptionToString(intrinsicValue, 1, "%.4f") + "\n")
      .append(s"Actual price to Intrinsic value, %  " + decimalOptionToString(actualValueToIntrinsicValuePercent(), 1, "%+.0f") + "\n")
      .toString

  //    if (List(debtToEquity, roe, intrinsicValue, actualPrice, actualValueToIntrinsicValuePercent())
  //      .forall(_.isDefined)) {
  //      new StringBuilder()
  //        .append(s"$ticker - $name\n")
  //        .append(s"Debt to equity, %                   " + debtToEquity.get * 100 + "\n")
  //        .append(s"ROE, %                              " + roe.get * 100 + "\n")
  //        .append(s"P/E                                 " + peRatio.get * 100 + "\n")
  //        .append(s"Actual price                        " + actualPrice.get + "\n")
  //        .append(s"Intrinsic value                     " + intrinsicValue.get.formatted("%.4f") + "\n")
  //        .append(s"Actual price to Intrinsic value, %  " + (if (actualValueToIntrinsicValuePercent().get >= 0) "+" else "") + actualValueToIntrinsicValuePercent().get.formatted("%.0f") + "\n")
  //        .toString
  //    }
  //    else s"$ticker - $name\nNO DATA\n"

  //  def format(str: String) = String.format("%0$-10s", str)

  //  def formatLine(strings: String*) = strings.foldLeft("")((zero, next) => zero + String.format("%0$-10s", next))

  def toStringLine =
    formatLine(ticker,
      name.substring(0, Math.min(name.length - 1, 23)),
      decimalOptionToString(debtToEquity, 100, "%.2f"),
      decimalOptionToString(roe, 100, "%.2f"),
      decimalOptionToString(peRatio, 1, "%.2f"),
      decimalOptionToString(actualPrice, 1, "%.2f"),
      decimalOptionToString(intrinsicValue, 1, "%.2f"),
      decimalOptionToString(actualValueToIntrinsicValuePercent(), 1, "%.2f"))

  //    if (isComplete) {
  //    val p = (if (actualValueToIntrinsicValuePercent().get >= 0) "+" else "") + actualValueToIntrinsicValuePercent().get.formatted("%.1f")
  //    formatLine(ticker, name.substring(0, Math.min(name.length - 1, 23)),
  //      (debtToEquity.get * 100).formatted("%.2f"),
  //      (roe.get * 100).formatted("%.2f"),
  //      peRatio.get.formatted("%.2f"),
  //      actualPrice.get.formatted("%.2f"),
  //      intrinsicValue.get.formatted("%.2f"),
  //      p.toString)
  //  }
  //  else formatLine(ticker, name.substring(0, Math.min(name.length - 1, 23)))

  def missingFields =
    ((if (cashFlow.isEmpty) List("cashFlow") else Nil) ++
      (if (longTermGrowth.isEmpty) List("longTermGrowth") else Nil) ++
      (if (beta.isEmpty) List("beta") else Nil) ++
      (if (sharesOutstanding.isEmpty) List("sharesOutstanding") else Nil) ++
      (if (debtToEquity.isEmpty) List("debtToEquity") else Nil) ++
      (if (roe.isEmpty) List("roe") else Nil) ++
      (if (peRatio.isEmpty) List("peRatio") else Nil) ++
      (if (actualPrice.isEmpty) List("actualPrice") else Nil))
      .mkString(", ")
}
