package com.garallex.stocks

import Utils._

case class Stock(ticker: String,
                 name: String,
                 debtToEquity: Option[BigDecimal],
                 roe: Option[BigDecimal],
                 intrinsicValue: Option[BigDecimal],
                 actualPrice: Option[BigDecimal]) {
  def actualValueToIntrinsicValuePercent() =
    (actualPrice, intrinsicValue) match {
      case (_, None) | (None, _) => None
      case (Some(actualPriceValue), Some(intrinsicValueValue)) => Some(100 * (actualPriceValue / intrinsicValueValue - 1))
    }

  override def toString =
    if (List(debtToEquity, roe, intrinsicValue, actualPrice, actualValueToIntrinsicValuePercent())
      .forall(_.isDefined)) {
      new StringBuilder()
        .append(s"$ticker - $name\n")
        .append(s"Debt to equity, %                   " + debtToEquity.get * 100 + "\n")
        .append(s"ROE, %                              " + roe.get * 100 + "\n")
        .append(s"Actual price                        " + actualPrice.get + "\n")
        .append(s"Intrinsic value                     " + intrinsicValue.get.formatted("%.4f") + "\n")
        .append(s"Actual price to Intrinsic value, %  " + (if (actualValueToIntrinsicValuePercent().get >= 0) "+" else "") + actualValueToIntrinsicValuePercent().get.formatted("%.0f") + "\n")
        .toString
    }
    else s"$ticker - $name\nNO DATA\n"

//  def format(str: String) = String.format("%0$-10s", str)

//  def formatLine(strings: String*) = strings.foldLeft("")((zero, next) => zero + String.format("%0$-10s", next))

  def toStringLine =
    if (List(debtToEquity, roe, intrinsicValue, actualPrice, actualValueToIntrinsicValuePercent())
      .forall(_.isDefined)) {
      val p = (if (actualValueToIntrinsicValuePercent().get >= 0) "+" else "") + actualValueToIntrinsicValuePercent().get.formatted("%.1f")
      formatLine(ticker, name.substring(0, Math.min(name.length - 1, 24)),
        (debtToEquity.get * 100).formatted("%.2f"),
        (roe.get * 100).formatted("%.2f"),
        actualPrice.get.formatted("%.2f"),
        intrinsicValue.get.formatted("%.2f"),
        p.toString)
    }
    else formatLine(ticker, name.substring(0, Math.min(name.length - 1, 24)))

}
