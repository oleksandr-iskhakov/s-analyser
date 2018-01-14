package com.garallex.stocks.domain

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
                 netIncomeAfterTax: Option[BigDecimal],
                 roe: Option[BigDecimal],
                 actualPrice: Option[BigDecimal],
                 peRatio: Option[BigDecimal],
                 eps: Option[BigDecimal],
                 currentRatio: Option[BigDecimal],
                 bookPerShare: Option[BigDecimal],
                 priceToBook: Option[BigDecimal],
                 enterpriseValue: Option[BigDecimal],
                 totalCurrentAssets: Option[BigDecimal],
                 totalCurrentLiabilities: Option[BigDecimal],
                 longTermDebt: Option[BigDecimal],
                 shortTermDebt: Option[BigDecimal],
                 cashAndShortTermInvestments: Option[BigDecimal],
                 totalDebt: Option[BigDecimal],
                 cashPerShare: Option[BigDecimal],
                 heldByInstitutionsRatio: Option[BigDecimal]) extends IntrinsicValueWa {

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

  lazy val longTermDebtToNetIncomeAfterTax: Option[BigDecimal] = (longTermDebt, netIncomeAfterTax) match {
    case (Some(ltd), Some(ni)) => Some(ltd / ni)
    case _ => None
  }

  lazy val marketCap: Option[BigDecimal] = (sharesOutstanding, actualPrice) match {
    case (Some(so), Some(p)) => Some(so * p)
    case _ => None
  }

  lazy val netCurrentAssets: Option[BigDecimal] = (totalCurrentAssets, totalCurrentLiabilities) match {
    case (Some(a), Some(l)) => Some(a - l)
    case _ => None
  }

  def missingFields: String =
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
