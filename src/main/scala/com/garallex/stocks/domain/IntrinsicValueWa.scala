package com.garallex.stocks.domain

trait IntrinsicValueWa {
  this: Stock =>

  lazy val intrinsicValueWa: Option[BigDecimal] =
    (cashFlowFromOperations,
      longTermGrowth,
      beta,
      sharesOutstanding,
      longTermDebt,
      shortTermDebt,
      cashAndShortTermInvestments) match {
      case (Some(cffo), Some(ltg), Some(b), Some(so), Some(ltd), Some(std), Some(csti)) =>
        Some(calcIntrinsicValueAdamKhoo(cffo, ltg, b, so, ltd, std, csti))
      case _ => None
    }

  private def calcIntrinsicValueAdamKhoo(cashFlow: BigDecimal,
                                         longTermGrowthRate: BigDecimal,
                                         beta: BigDecimal,
                                         sharesOutstanding: BigDecimal,
                                         longTermDebt: BigDecimal,
                                         shortTermDebt: BigDecimal,
                                         cashAndStInvestments: BigDecimal): BigDecimal = {

    def getDiscountRateByBeta(beta: BigDecimal): BigDecimal =
      (if (beta < 0.8) BigDecimal("5")
      else if (beta < 1) BigDecimal("6")
      else if (beta < 1.1) BigDecimal("6")
      else if (beta < 1.2) BigDecimal("6.8")
      else if (beta < 1.3) BigDecimal("7")
      else if (beta < 1.4) BigDecimal("7.9")
      else if (beta < 1.5) BigDecimal("8")
      else if (beta < 1.6) BigDecimal("8.9")
      else BigDecimal("9")) / BigDecimal("100")

    val projectedCashFlow = new collection.mutable.ArrayBuffer[BigDecimal](11)
    val discountFactor = new collection.mutable.ArrayBuffer[BigDecimal](11)
    val discountedValue = new collection.mutable.ArrayBuffer[BigDecimal](11)

    val discountRate = getDiscountRateByBeta(beta)
    projectedCashFlow.append(cashFlow)
    discountFactor.append(0)
    discountedValue.append(projectedCashFlow(0) * discountFactor(0))

    for (i <- 1 to 10) {
      val applicableGrowthRate = if (i > 3 && longTermGrowthRate > BigDecimal("0.15"))
        BigDecimal("0.15")
      else
        longTermGrowthRate

      projectedCashFlow.append(projectedCashFlow(i - 1) * (1 + applicableGrowthRate))
      discountFactor.append(if (i == 1) 1 / (1 + discountRate) else discountFactor(i - 1) / (1 + discountRate))
      discountedValue.append(projectedCashFlow(i) * discountFactor(i))
    }

    val pv10years = discountedValue.drop(1).sum
    val ivBeforeCashDebt = pv10years / sharesOutstanding
    val debtPerShare = (longTermDebt + shortTermDebt) / sharesOutstanding
    val cashPerShare = cashAndStInvestments / sharesOutstanding
    ivBeforeCashDebt - debtPerShare + cashPerShare
  }
}
