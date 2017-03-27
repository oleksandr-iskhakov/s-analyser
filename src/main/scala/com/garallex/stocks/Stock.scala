package com.garallex.stocks

import com.garallex.stocks.Utils._

import scala.util.{Success, Try}

case class Stock(ticker: String,
                 name: String,
                 industry: String,
                 cashFlowFromOperations: Option[BigDecimal],
                 freeCashFlow: Option[BigDecimal],
                 longTermGrowth: Option[BigDecimal],
                 beta: Option[BigDecimal],
                 sharesOutstanding: Option[BigDecimal],
                 debtToEquity: Option[BigDecimal],
                 roe: Option[BigDecimal],
                 actualPrice: Option[BigDecimal],
                 peRatio: Option[BigDecimal],
                 eps: Option[BigDecimal],
                 currentRatio: Option[BigDecimal],
                 bookPerShare: Option[BigDecimal],
                 priceToBook: Option[BigDecimal],
                 enterpriseValue: Option[BigDecimal],
                 totalCurrentAssets: Option[BigDecimal],
                 longTermDebt: Option[BigDecimal],
                 totalDebt: Option[BigDecimal],
                 cashPerShare: Option[BigDecimal]) {

  //  lazy val intrinsicValueAdamKhooOriginal: Option[BigDecimal] =
  //    (cashFlowFromOperations, longTermGrowth, beta, sharesOutstanding, totalDebt, cashPerShare) match {
  //      case (Some(cf), Some(ltg), Some(b), Some(so), Some(td), Some(cps)) =>
  //        Some(calcIntrinsicValueAdamKhoo(cf, ltg, b, so) - td / so + cps)
  //      case _ => None
  //    }

  lazy val intrinsicValueAdamKhooOnFreeCashFlow: Option[BigDecimal] =
    (freeCashFlow, longTermGrowth, beta, sharesOutstanding, totalDebt, cashPerShare) match {
      case (Some(fcf), Some(ltg), Some(b), Some(so), Some(td), Some(cps)) =>
        Some(calcIntrinsicValueAdamKhoo(fcf, ltg, b, so) - td / so + cps)
      case _ => None
    }

  lazy val intrinsicValueAdamKhooOriginal: Option[BigDecimal] =
    (cashFlowFromOperations, longTermGrowth, beta, sharesOutstanding) match {
      case (Some(cf), Some(ltg), Some(b), Some(so)) => Some(calcIntrinsicValueAdamKhoo(cf, ltg, b, so))
      case _ => None
    }

  lazy val intrinsicValueGraham: Option[BigDecimal] = (eps, longTermGrowth) match {
    case (Some(epsValue), Some(longTermGrowthValue)) => Some(epsValue * (8.5 + 2 * longTermGrowthValue * 100))
    case _ => None
  }

  lazy val intrinsicValueGrahamUpdated: Option[BigDecimal] = intrinsicValueGraham match {
    case Some(value) => Some(value * 4 / 3.7)
    case None => None
  }

  lazy val grahamNumber: Option[BigDecimal] =
    (eps, bookPerShare) match {
      case (Some(epsValue), Some(bookPerShareValue)) => Try {
        BigDecimal(Math.sqrt(15 * 1.5 * epsValue.toDouble * bookPerShareValue.toDouble))
      } match {
        case Success(result) => Some(result)
        case _ => None
      }
      case _ => None
    }

  lazy val grahamMixedMultiplier: Option[BigDecimal] = (peRatio, priceToBook) match {
    case (Some(peRatioValue), Some(priceToBookValue)) => Some(peRatioValue * priceToBookValue)
    case _ => None
  }

  private def calcIntrinsicValueAdamKhoo(cashFlow: BigDecimal,
                                         longTermGrowthRate: BigDecimal,
                                         beta: BigDecimal,
                                         sharesOutstanding: BigDecimal): BigDecimal = {

    def getDiscountRateByBeta(beta: BigDecimal): BigDecimal =
      (if (beta < 0.8) BigDecimal("5")
      else if (beta < 1) BigDecimal("5.7") // ???
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
    val discountRate = getDiscountRateByBeta(beta)
    for (i <- p.indices) p.insert(i, p(i) / math.pow((1 + discountRate).toDouble, i + 1))
    p.sum / sharesOutstanding
  }

  private def decimalOptionToString(value: Option[BigDecimal],
                                    multiplier: BigDecimal = 1,
                                    format: String = "") = value match {
    case Some(v) if format == "" => (v * multiplier).toString()
    case Some(v) => (v * multiplier).formatted(format)
    case _ => ""
  }

  override def toString: String =
    new StringBuilder()
      .append(s"$ticker - $name - $industry\n")
      .append(s"Enterprise Value                    ${decimalOptionToString(enterpriseValue)}\n")
      .append(s"Total Current Assets                ${decimalOptionToString(totalCurrentAssets)}\n")
      .append(s"Long Term Debt                      ${decimalOptionToString(longTermDebt)}\n")
      .append(s"Total Debt                          ${decimalOptionToString(totalDebt)}\n")
      .append(s"Debt to equity, %                   ${decimalOptionToString(debtToEquity, 100)} (should be < 50)\n")
      .append(s"ROE, %                              ${decimalOptionToString(roe, 100)} (should be > 15)\n")
      .append(s"P/E                                 ${decimalOptionToString(peRatio)} (should be < 15)\n")
      .append(s"Cash flow Form Operations           ${decimalOptionToString(cashFlowFromOperations)}\n")
      .append(s"Free Cash Flow                      ${decimalOptionToString(freeCashFlow)}\n")
      .append(s"Cash per share                      ${decimalOptionToString(cashPerShare)}\n")
      .append(s"Long term growth                    ${decimalOptionToString(longTermGrowth)}\n")
      .append(s"Beta                                ${decimalOptionToString(beta)}\n")
      .append(s"Shares outstanding                  ${decimalOptionToString(sharesOutstanding)}\n")
      .append(s"EPS                                 ${decimalOptionToString(eps)}\n")
      .append(s"Current Ratio                       ${decimalOptionToString(currentRatio)} (should be >= 1.5)\n")
      .append(s"Book per share                      ${decimalOptionToString(bookPerShare)}\n")
      .append(s"Price to book                       ${decimalOptionToString(priceToBook)}\n")
      .append(s"Actual price                        ${decimalOptionToString(actualPrice)}\n")
      .append(s"Intrinsic A.Khoo Original           ${decimalOptionToString(intrinsicValueAdamKhooOriginal, 1, "%.4f")}\n")
      .append(s"Intrinsic A.Khoo on Free Cash Flow  ${decimalOptionToString(intrinsicValueAdamKhooOnFreeCashFlow, 1, "%.4f")}\n")
      .append(s"Intrinsic value Graham              ${decimalOptionToString(intrinsicValueGraham, 1, "%.4f")}\n")
      .append(s"Intrinsic value Graham (Updated)    ${decimalOptionToString(intrinsicValueGrahamUpdated, 1, "%.4f")}\n")
      .append(s"Graham Mixed Multiplier (P/E * P/B) ${decimalOptionToString(grahamMixedMultiplier, 1, "%.4f")} (must be <= 22.5)\n")
      .append(s"Graham Number                       ${decimalOptionToString(grahamNumber, 1, "%.4f")}\n")
      .toString

//  def toStringLine =
//    formatLine(ticker,
//      name.substring(0, Math.min(name.length - 1, 23)),
//      industry,
//      decimalOptionToString(debtToEquity, 100, "%.2f"),
//      decimalOptionToString(roe, 100, "%.2f"),
//      decimalOptionToString(peRatio, 1, "%.2f"),
//      decimalOptionToString(actualPrice, 1, "%.2f"),
//      decimalOptionToString(intrinsicValueAdamKhooOriginal, 1, "%.2f"))

  def missingFields =
    ((if (cashFlowFromOperations.isEmpty) List("cashFlow") else Nil) ++
      (if (freeCashFlow.isEmpty) List("freeCashFlow") else Nil) ++
      (if (longTermGrowth.isEmpty) List("longTermGrowth") else Nil) ++
      (if (beta.isEmpty) List("beta") else Nil) ++
      (if (sharesOutstanding.isEmpty) List("sharesOutstanding") else Nil) ++
      (if (debtToEquity.isEmpty) List("debtToEquity") else Nil) ++
      (if (roe.isEmpty) List("roe") else Nil) ++
      (if (peRatio.isEmpty) List("peRatio") else Nil) ++
      (if (actualPrice.isEmpty) List("actualPrice") else Nil) ++
      (if (eps.isEmpty) List("eps") else Nil) ++
      (if (currentRatio.isEmpty) List("currentRatio") else Nil) ++
      (if (bookPerShare.isEmpty) List("bookPerShare") else Nil) ++
      (if (priceToBook.isEmpty) List("priceToBook") else Nil))
      .mkString(", ")
}
