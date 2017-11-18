package com.garallex.stocks

import com.garallex.stocks.criteria._
import com.garallex.stocks.domain.Stock

class StockToString(stock: Stock) {
  private def decimalOptionToString(value: Option[BigDecimal],
                                    multiplier: BigDecimal = 1,
                                    format: String = "") = value match {
    case Some(v) if format == "" => (v * multiplier).toString()
    case Some(v) => (v * multiplier).formatted(format)
    case _ => ""
  }

  override def toString: String =
    new StringBuilder()
      .append(s"${stock.ticker} - ${stock.name} - ${stock.industry}\n")
      .append(s"Market Cap                            ${decimalOptionToString(stock.marketCap, 1, "%.0f")} (should be >= 10 B. Graham). ${MarketCap(stock)}\n")
      .append(s"Enterprise Value                      ${decimalOptionToString(stock.enterpriseValue)} (should be >= 2 B. Graham). ${EnterpriseValue(stock)}\n")
      .append(s"Shares outstanding                    ${decimalOptionToString(stock.sharesOutstanding)}\n")
      .append(s"Total Current Assets                  ${decimalOptionToString(stock.totalCurrentAssets)}\n")
      .append(s"Total Current Liabilities             ${decimalOptionToString(stock.totalCurrentLiabilities)}\n")
      .append(s"Net Current Assets                    ${decimalOptionToString(stock.netCurrentAssets)} (should be >= Long Term Debt. Graham). ${NetCurrentAssetsToLongTermDebt(stock)}\n")
      .append(s"Cash flow Form Operations             ${decimalOptionToString(stock.cashFlowFromOperations)}\n")
      .append(s"Free Cash Flow                        ${decimalOptionToString(stock.freeCashFlow)}\n")
      .append(s"Long Term Debt                        ${decimalOptionToString(stock.longTermDebt)}\n")
      .append(s"Total Debt                            ${decimalOptionToString(stock.totalDebt)}\n")
      .append(s"Net Income After Tax                  ${decimalOptionToString(stock.netIncomeAfterTax)}\n")
      .append(s"Long Term Debt / Net Income After Tax ${decimalOptionToString(stock.longTermDebtToNetIncomeAfterTax, 1, "%.1f")} (should be < 3. Khoo). ${LongTermDebtToNetIncomeAfterTax(stock)} \n")
      .append(s"Debt to equity, %                     ${decimalOptionToString(stock.debtToEquity, 100, "%.1f")} (should be < 50. Graham & Khoo). ${DebtToEquity(stock)}\n")
      .append(s"ROE, %                                ${decimalOptionToString(stock.roe, 100, "%.1f")} (should be > 12-15. Khoo). ${Roe(stock)}\n")
      .append(s"P/E                                   ${decimalOptionToString(stock.peRatio)} (should be < 15. Graham). ${PriceToEarnings(stock)}\n")
      .append(s"Cash per share                        ${decimalOptionToString(stock.cashPerShare)}\n")
      .append(s"Long term growth, %                   ${decimalOptionToString(stock.longTermGrowth, 100, "%.1f")} (should be >= 10%. Khoo). ${LongTermGrowthProspects(stock)}\n")
      .append(s"Beta                                  ${decimalOptionToString(stock.beta)}\n")
      .append(s"EPS                                   ${decimalOptionToString(stock.eps)} (should have grown for 33% during the latest 10 years. Even more: 50%-100% (4-7% per year). Passive. Graham)\n")
      .append(s"Current Ratio                         ${decimalOptionToString(stock.currentRatio)} (should be >= 2 for Passive, >= 1.5 for Active. Graham). Active: ${CurrentRatioActive(stock)}. Passive: ${CurrentRatioPassive(stock)}\n")
      .append(s"Book per share                        ${decimalOptionToString(stock.bookPerShare)}\n")
      .append(s"Price to book                         ${decimalOptionToString(stock.priceToBook, 1, "%.1f")} (should be <= 1.5. Graham). ${PriceToBook(stock)}\n")
      .append(s"\n")
      .append(s"Actual price                          ${decimalOptionToString(stock.actualPrice)}\n")
      .append(s"Intrinsic A.Khoo Original             ${decimalOptionToString(stock.intrinsicValueAdamKhooOriginal, 1, "%.2f")} (should be at least 20% discount. Khoo). ${IntrinsicValueAdamKhooOriginal(stock)}\n")
      .append(s"Intrinsic A.Khoo on Free Cash Flow    ${decimalOptionToString(stock.intrinsicValueAdamKhooOnFreeCashFlow, 1, "%.2f")} (should be at least 20% discount. Khoo). ${IntrinsicValueAdamKhooOnFreeCashFlow(stock)}\n")
      .append(s"Intrinsic value Graham                ${decimalOptionToString(stock.intrinsicValueGraham, 1, "%.2f")} (should be at least 20% discount. Khoo). ${IntrinsicValueGraham(stock)}\n")
      .append(s"Intrinsic value Graham (Updated)      ${decimalOptionToString(stock.intrinsicValueGrahamUpdated, 1, "%.2f")} (should be at least 20% discount. Khoo). ${IntrinsicValueGrahamUpdated(stock)}\n")
      .append(s"Graham Mixed Multiplier (P/E * P/B)   ${decimalOptionToString(stock.grahamMixedMultiplier, 1, "%.2f")} (should be <= 22.5. Graham)\n")
      .append(s"Graham Number                         ${decimalOptionToString(stock.grahamNumber, 1, "%.2f")} (the upper bound of the price range that a defensive investor should pay for the stock. Graham)\n")
      .append(s"\n")
      .append(s"Held by Institutions, %               ${decimalOptionToString(stock.heldByInstitutionsRatio, 100, "%.1f")} (Should be < 60 %. Graham. Value more then 60% is considered as institutionally overbought)\n")
      .toString

}
