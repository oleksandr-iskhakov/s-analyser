package com.garallex.stocks

import java.net.URL
import java.util.concurrent.ConcurrentHashMap

import com.garallex.stocks.domain.Stock
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source

class StockBuilder(ticker: String, name: String = "", industry: String = "") {
  val pageDocumentCache = new ConcurrentHashMap[String, Document]
  val pageStringCache = new ConcurrentHashMap[String, String]

  def fetchWebPage(ticker: String, prefix: String, urlBuilder: (String, String) => URL): Document = {
    fetchWebPageAsDocument(urlBuilder(ticker, prefix))
  }

  def fetchWebPageAsDocument(url: URL): Document = {
    if (!pageDocumentCache.containsKey(url.toString))
      pageDocumentCache.putIfAbsent(url.toString, Jsoup.parse(url, 4000))
    pageDocumentCache.get(url.toString)
  }

  def fetchWebPageAsString(url: URL): String = {
    if (!pageStringCache.containsKey(url.toString))
      pageStringCache.putIfAbsent(url.toString, Source.fromInputStream(url.openConnection().getInputStream).mkString)
    pageStringCache.get(url.toString)
  }


  def abbreviatedStringToBigDecimal(value: String): BigDecimal = value.last match {
    case 'K' => BigDecimal(value.split('K').head) * 1000
    case 'M' => BigDecimal(value.split('M').head) * 1000000
    case 'B' => BigDecimal(value.split('B').head) * 1000000000
    case n if n.isDigit => BigDecimal(value)
    case u => throw new Exception(s"Unrecognized multiplier $u")
  }

  //  def gotoParentTagName(elem: Element, searchTag: String): Element =
  //    if (elem != null) {
  //      if (elem.tagName == searchTag) elem
  //      else gotoParentTagName(elem.parent, searchTag)
  //    }
  //    else null

  def fetchPERatioCNN(ticker: String): BigDecimal = {
    val doc = fetchWebPageAsDocument(new URL(s"http://money.cnn.com/quote/quote.html?symb=$ticker"))
    val elements = doc.body.getElementsContainingOwnText("P/E ratio")
    val td = elements.get(0)
    val element = td.parent.children.get(1)
    abbreviatedStringToBigDecimal(element.ownText)
  }

  def fetchCashFlowFromOperationsYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"operatingCashflow\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchCashFlowFromOperationsCNN(ticker: String): BigDecimal = {
    val doc = fetchWebPageAsDocument(new URL(s"http://money.cnn.com/quote/financials/financials.html?dataSet=CFS&symb=$ticker"))
    val elements = doc.body.getElementsContainingOwnText("Net Cash Flow - Operating Activities")
    val td = elements.get(0)
    val element = td.parent.children.get(4)
    abbreviatedStringToBigDecimal(element.ownText)
  }

  def fetchFreeCashFlowWSJ(ticker: String): BigDecimal = {
    val doc = fetchWebPageAsDocument(new URL(s"http://quotes.wsj.com/$ticker/financials/annual/cash-flow"))
    val multiplier =
      if (!doc.body.getElementsContainingText("All values USD Thousands").isEmpty) BigDecimal(1000)
      else if (!doc.body.getElementsContainingText("All values USD Millions").isEmpty) BigDecimal(1000000)
      else throw new Exception("fetchFreeCashFlowWSJ(): Unknown multiplier")

    val elements = doc.body.getElementsContainingOwnText("Free Cash Flow")
    val td = elements.get(0)
    val element = td.parent.children.get(1)
    abbreviatedStringToBigDecimal(element.ownText.replaceAll(",", "")) * multiplier
  }

  def fetchFreeCashFlowMarketWatch(ticker: String): BigDecimal = {
    val doc = fetchWebPageAsDocument(new URL(s"http://www.marketwatch.com/investing/stock/$ticker/financials/cash-flow"))
    val elements = doc.body.getElementsContainingOwnText("Free Cash Flow")
    val children = elements.get(0).parent().children()
    val element = children.get(children.size() - 2)
    abbreviatedStringToBigDecimal(element.ownText)
  }

  def isDigitOrDotOrMinus(c: Char): Boolean = c.isDigit || c == '.' || c == '-'

  def findByKey(json: String, key: String): String = {
    var ndx = json.indexOf(key) + key.length
    val sb = new StringBuilder
    while (isDigitOrDotOrMinus(json.charAt(ndx))) {
      sb.append(json.charAt(ndx))
      ndx += 1
    }
    sb.toString()
  }

  def fetchLongTermGrowthRateYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query2.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=upgradeDowngradeHistory%2CrecommendationTrend%2CfinancialData%2CearningsHistory%2CearningsTrend%2CindustryTrend")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "{\"maxAge\":1,\"period\":\"+5y\",\"endDate\":null,\"growth\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchSharesOutstandingYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"sharesOutstanding\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchBetaYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"beta\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchROERateYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query2.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=upgradeDowngradeHistory%2CrecommendationTrend%2CfinancialData%2CearningsHistory%2CearningsTrend%2CindustryTrend")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"returnOnEquity\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchTotalDebtToEquityRateYahoo(ticker: String): BigDecimal = {
    // it is in percent.
    // prove found at http://www.gurufocus.com/term/deb2equity/V/Debt-to-Equity/Visa-Inc for VISA and compared you YAHOO data
    val url = new URL(s"https://query2.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=upgradeDowngradeHistory%2CrecommendationTrend%2CfinancialData%2CearningsHistory%2CearningsTrend%2CindustryTrend")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"debtToEquity\":{\"raw\":")
    BigDecimal(value) / BigDecimal(100)
  }

  def fetchTotalDebtToEquityRateReuters(ticker: String): BigDecimal = {
    val doc = fetchWebPageAsDocument(new URL(s"http://www.reuters.com/finance/stocks/financialHighlights?symbol=$ticker"))
    val elements = doc.body.getElementsContainingOwnText("Total Debt to Equity (MRQ)")
    val td = elements.get(0)
    val element = td.parent.children.get(1)
    abbreviatedStringToBigDecimal(element.ownText) / 100
  }

  def fetchEpsYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"trailingEps\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchEnterpriseValueYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"enterpriseValue\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchHeldByInstitutionsRatioYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"heldPercentInstitutions\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchCurrentRatioYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"currentRatio\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchPriceToBookYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"priceToBook\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchBookPerShareYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"bookValue\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchCashPerShareYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"totalCashPerShare\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchTotalDebtYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"totalDebt\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchNetIncomeAfterTax(ticker: String): BigDecimal = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"netIncomeToCommon\":{\"raw\":")
    BigDecimal(value)
  }


  def fetchPrevCloseYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query2.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=upgradeDowngradeHistory%2CrecommendationTrend%2CfinancialData%2CearningsHistory%2CearningsTrend%2CindustryTrend")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"currentPrice\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchTotalCurrentAssetsYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query2.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=incomeStatementHistory%2CcashflowStatementHistory%2CbalanceSheetHistory%2CincomeStatementHistoryQuarterly%2CcashflowStatementHistoryQuarterly%2CbalanceSheetHistoryQuarterly%2Cearnings")
    val jsonString = fetchWebPageAsString(url)
    val jSon = parse(jsonString)
    val value = ((jSon \ "quoteSummary" \ "result") (0) \ "balanceSheetHistory" \ "balanceSheetStatements") (0) \ "totalCurrentAssets" \ "raw"
    implicit val formats = DefaultFormats
    BigDecimal(value.extract[BigInt])
  }

  def fetchTotalCurrentLiabilitiesYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query2.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=incomeStatementHistory%2CcashflowStatementHistory%2CbalanceSheetHistory%2CincomeStatementHistoryQuarterly%2CcashflowStatementHistoryQuarterly%2CbalanceSheetHistoryQuarterly%2Cearnings")
    val jsonString = fetchWebPageAsString(url)
    val jSon = parse(jsonString)
    val value = ((jSon \ "quoteSummary" \ "result") (0) \ "balanceSheetHistory" \ "balanceSheetStatements") (0) \ "totalCurrentLiabilities" \ "raw"
    implicit val formats = DefaultFormats
    BigDecimal(value.extract[BigInt])
  }

  def fetchLongTermDebtYahoo(ticker: String): BigDecimal = {
    val url = new URL(s"https://query2.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=incomeStatementHistory%2CcashflowStatementHistory%2CbalanceSheetHistory%2CincomeStatementHistoryQuarterly%2CcashflowStatementHistoryQuarterly%2CbalanceSheetHistoryQuarterly%2Cearnings")
    val jsonString = fetchWebPageAsString(url)
    val jSon = parse(jsonString)
    val value = ((jSon \ "quoteSummary" \ "result") (0) \ "balanceSheetHistory" \ "balanceSheetStatements") (0) \ "longTermDebt" \ "raw"
    implicit val formats = DefaultFormats
    BigDecimal(value.extract[BigInt])
  }

  def fetchStockTickers(): Iterator[(String, String, String)] = {
    val url = new URL(s"https://raw.githubusercontent.com/datasets/s-and-p-500-companies/master/data/constituents.csv")
    val tickers = fetchWebPageAsString(url)
    tickers
      .lines
      .drop(1)
      .map {
        line => {
          val split = line.split(',')
          (split.head, split(1), split.last)
        }
      }
  }

  def fetchValue1[T](ticker: String, fetcher1: String => T): Future[Option[T]] =
    Future[Option[T]] {
      Some[T](fetcher1(ticker))
    } recoverWith {
      case _ => Future[Option[T]] {
        None
      }
    }

  def fetchValue2[T](ticker: String, fetcher1: String => T, fetcher2: String => T): Future[Option[T]] =
    Future[Option[T]] {
      Some[T](fetcher1(ticker))
    } recoverWith {
      case _ => Future[Option[T]] {
        Some[T](fetcher2(ticker))
      }
    } recoverWith {
      case _ => Future[Option[T]] {
        None
      }
    }

  def fetchValue[T](ticker: String, fetchers: (String => T)*): Future[Option[T]] =
    fetchers.toList match {
      case h :: Nil => fetchValue1(ticker, h)
      case h1 :: h2 :: Nil => fetchValue2(ticker, h1, h2)
      case _ => throw new NotImplementedError()
    }

  def build(): Stock = {
    val stockFuture = for {
      cashFlowFromOperations <- fetchValue(ticker, fetchCashFlowFromOperationsYahoo, fetchCashFlowFromOperationsCNN)
      freeCashFlow <- fetchValue(ticker, fetchFreeCashFlowMarketWatch, fetchFreeCashFlowWSJ)
      longTermGrowth <- fetchValue(ticker, fetchLongTermGrowthRateYahoo)
      beta <- fetchValue(ticker, fetchBetaYahoo)
      sharesOutstanding <- fetchValue(ticker, fetchSharesOutstandingYahoo)
      debtToEquity <- fetchValue(ticker, fetchTotalDebtToEquityRateYahoo, fetchTotalDebtToEquityRateReuters)
      roe <- fetchValue(ticker, fetchROERateYahoo)
      peRatio <- fetchValue(ticker, fetchPERatioCNN)
      actualPrice <- fetchValue(ticker, fetchPrevCloseYahoo)
      eps <- fetchValue(ticker, fetchEpsYahoo)
      currentRatio <- fetchValue(ticker, fetchCurrentRatioYahoo)
      bookPerShare <- fetchValue(ticker, fetchBookPerShareYahoo)
      priceToBook <- fetchValue(ticker, fetchPriceToBookYahoo)
      enterpriseValue <- fetchValue(ticker, fetchEnterpriseValueYahoo)
      totalCurrentAssets <- fetchValue(ticker, fetchTotalCurrentAssetsYahoo)
      totalCurrentLiabilities <- fetchValue(ticker, fetchTotalCurrentLiabilitiesYahoo)
      longTermDebt <- fetchValue(ticker, fetchLongTermDebtYahoo)
      totalDebt <- fetchValue(ticker, fetchTotalDebtYahoo)
      cashPerShare <- fetchValue(ticker, fetchCashPerShareYahoo)
      netIncomeAfterTax <- fetchValue(ticker, fetchNetIncomeAfterTax)
      heldByInstitutionsRatio <- fetchValue(ticker, fetchHeldByInstitutionsRatioYahoo)
    } yield Stock(
      ticker = ticker,
      name = name,
      industry = industry,
      cashFlowFromOperations = cashFlowFromOperations,
      freeCashFlow = freeCashFlow,
      debtToEquity = debtToEquity,
      roe = roe,
      peRatio = peRatio,
      longTermGrowth = longTermGrowth,
      beta = beta,
      sharesOutstanding = sharesOutstanding,
      actualPrice = actualPrice,
      eps = eps,
      currentRatio = currentRatio,
      bookPerShare = bookPerShare,
      priceToBook = priceToBook,
      enterpriseValue = enterpriseValue,
      totalCurrentAssets = totalCurrentAssets,
      totalCurrentLiabilities = totalCurrentLiabilities,
      longTermDebt = longTermDebt,
      totalDebt = totalDebt,
      cashPerShare = cashPerShare,
      netIncomeAfterTax = netIncomeAfterTax,
      heldByInstitutionsRatio = heldByInstitutionsRatio)

    Await.result(stockFuture, 10 minutes)
  }
}
