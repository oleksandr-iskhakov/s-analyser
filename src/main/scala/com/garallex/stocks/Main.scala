package com.garallex.stocks

import java.net.URL
import java.util.concurrent.ConcurrentHashMap

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import yahoofinance.YahooFinance

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Main {
  def buildYahooFinanceUrl(ticker: String, prefix: String = null) = {
    val result = new URL(s"http://finance.yahoo.com/quote/$ticker/$prefix")
    println(result)
    result
  }

  def buildReutersUrl(ticker: String, prefix: String) =
    new URL(s"http://www.reuters.com/finance/stocks/$prefix?symbol=$ticker")

  val pageDocumentCache = new ConcurrentHashMap[String, Document]
  val pageStringCache = new ConcurrentHashMap[String, String]

  def fetchWebPage(ticker: String, prefix: String, urlBuilder: (String, String) => URL): Document = {
    fetchWebPageAsDocument(urlBuilder(ticker, prefix))
  }

  def fetchWebPageAsDocument(url: URL) = {
    if (!pageDocumentCache.containsKey(url.toString))
      pageDocumentCache.putIfAbsent(url.toString, Jsoup.parse(url, 4000))
    pageDocumentCache.get(url.toString)
  }

  def fetchWebPageAsString(url: URL) = {
    if (!pageStringCache.containsKey(url.toString))
      pageStringCache.putIfAbsent(url.toString, Source.fromInputStream(url.openConnection().getInputStream).mkString)
    pageStringCache.get(url.toString)
  }


  def abbreviatedStringToBigDecimal(value: String) = value.last match {
    case 'K' => BigDecimal(value.split('K').head) * 1000
    case 'M' => BigDecimal(value.split('M').head) * 1000000
    case 'B' => BigDecimal(value.split('B').head) * 1000000000
    case n if n.isDigit => BigDecimal(value)
    case u => throw new Exception(s"Unrecognized multiplier $u")
  }

  def gotoParentTagName(elem: Element, searchTag: String): Element =
    if (elem != null) {
      if (elem.tagName == searchTag) elem
      else gotoParentTagName(elem.parent, searchTag)
    }
    else null

  def fetchPERatioCNN(ticker: String) = {
    val doc = fetchWebPageAsDocument(new URL(s"http://money.cnn.com/quote/quote.html?symb=$ticker"))
    val elements = doc.body.getElementsContainingOwnText("P/E ratio")
    val td = elements.get(0)
    val element = td.parent.children.get(1)
    abbreviatedStringToBigDecimal(element.ownText)
  }

  def fetchCashFlowFromOperationsYahoo(ticker: String) = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"operatingCashflow\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchCashFlowFromOperationsCNN(ticker: String) = {
    val doc = fetchWebPageAsDocument(new URL(s"http://money.cnn.com/quote/financials/financials.html?dataSet=CFS&symb=$ticker"))
    val elements = doc.body.getElementsContainingOwnText("Net Cash Flow - Operating Activities")
    val td = elements.get(0)
    val element = td.parent.children.get(4)
    abbreviatedStringToBigDecimal(element.ownText)
  }

  def isDigitOrDotOrMinus(c: Char) = c.isDigit || c == '.' || c == '-'

  def findByKey(json: String, key: String) = {
    var ndx = json.indexOf(key) + key.length
    val sb = new StringBuilder
    while (isDigitOrDotOrMinus(json.charAt(ndx))) {
      sb.append(json.charAt(ndx))
      ndx += 1
    }
    sb.toString()
  }

  def fetchLongTermGrowthRateYahoo(ticker: String) = {
    val url = new URL(s"https://query2.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=upgradeDowngradeHistory%2CrecommendationTrend%2CfinancialData%2CearningsHistory%2CearningsTrend%2CindustryTrend")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "{\"maxAge\":1,\"period\":\"+5y\",\"endDate\":null,\"growth\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchSharesOutstandingYahoo(ticker: String) = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"sharesOutstanding\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchBetaYahoo(ticker: String) = {
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
    abbreviatedStringToBigDecimal(element.ownText)
  }

  def fetchEpsYahoo(ticker: String) = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"trailingEps\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchCurrentRatioYahoo(ticker: String) = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"currentRatio\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchPriceToBookYahoo(ticker: String) = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"priceToBook\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchBookPerShareYahoo(ticker: String) = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"bookValue\":{\"raw\":")
    BigDecimal(value)
  }


  def fetchPrevCloseYahoo(ticker: String) = {
    val url = new URL(s"https://query2.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=upgradeDowngradeHistory%2CrecommendationTrend%2CfinancialData%2CearningsHistory%2CearningsTrend%2CindustryTrend")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"currentPrice\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchStockTickers() = {
    val url = new URL(s"http://data.okfn.org/data/core/s-and-p-500-companies/r/constituents.csv")
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
    } recoverWith { case _ => Future[Option[T]] {
      None
    }
    }

  def fetchValue2[T](ticker: String, fetcher1: String => T, fetcher2: String => T): Future[Option[T]] =
    Future[Option[T]] {
      Some[T](fetcher1(ticker))
    } recoverWith { case _ => Future[Option[T]] {
      Some[T](fetcher2(ticker))
    }
    } recoverWith { case _ => Future[Option[T]] {
      None
    }
    }

  def fetchValue[T](ticker: String, fetchers: (String => T)*): Future[Option[T]] = fetchers.toList match {
    case h :: Nil => fetchValue1(ticker, h)
    case h1 :: h2 :: Nil => fetchValue2(ticker, h1, h2)
    case _ => throw new NotImplementedError()
  }

  def buildStock(ticker: String, name: String, industry: String) = {
    val cashFlowFuture = fetchValue[BigDecimal](ticker, fetchCashFlowFromOperationsYahoo, fetchCashFlowFromOperationsCNN)
    val longTermGrowthFuture = fetchValue(ticker, fetchLongTermGrowthRateYahoo)
    val betaFuture = fetchValue(ticker, fetchBetaYahoo)
    val sharesOutstandingFuture = fetchValue(ticker, fetchSharesOutstandingYahoo)
    val actualPriceFuture = fetchValue(ticker, fetchPrevCloseYahoo)
    val totalDebtToEquityRateFuture = fetchValue(ticker, fetchTotalDebtToEquityRateYahoo, fetchTotalDebtToEquityRateReuters)
    val roeRateFuture = fetchValue(ticker, fetchROERateYahoo)
    val peRatioFuture = fetchValue(ticker, fetchPERatioCNN)
    val epsFuture = fetchValue(ticker, fetchEpsYahoo)
    val currentRatioFuture = fetchValue(ticker, fetchCurrentRatioYahoo)
    val bookPerShareFuture = fetchValue(ticker, fetchBookPerShareYahoo)
    val priceToBookFuture = fetchValue(ticker, fetchPriceToBookYahoo)

    val stockFuture = for {
      cashFlow <- cashFlowFuture
      longTermGrowth <- longTermGrowthFuture
      beta <- betaFuture
      sharesOutstanding <- sharesOutstandingFuture
      debtToEquity <- totalDebtToEquityRateFuture
      roe <- roeRateFuture
      peRatio <- peRatioFuture
      actualPrice <- actualPriceFuture
      eps <- epsFuture
      currentRatio <- currentRatioFuture
      bookPerShare <- bookPerShareFuture
      priceToBook <- priceToBookFuture
    } yield Stock(
      ticker,
      name,
      industry,
      cashFlow = cashFlow,
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
      priceToBook = priceToBook)

    Await.result(stockFuture, 1 minutes)
  }


  def main(args: Array[String]) = {
        val stock = buildStock("V", "", "")
        println(stock)
    //
    //    import Utils._
    //
    //    println(formatLine("Ticker", "Name", "Industry", "D/E, %", "ROE, %", "P/E Ratio", "Actual price", "Intrinsic value", "A/I, %"))
    //    println()
    //    val tickers = fetchStockTickers()
    //      .toList
    //      .sortBy(_._3)
    //      .toParArray
    //
    //    val allStocks = tickers
    //      .par
    //      .map { case (ticker, name, industry) =>
    //        val stock = buildStock(ticker, name, industry)
    //        println(stock.toStringLine)
    //        stock
    //      }
    //    println("\n")
    //    println("Screen is positive:\n")
    //    val filteredStocks =
    //      allStocks.filter(stock => (stock.debtToEquity, stock.roe, stock.actualValueToIntrinsicValuePercent()) match {
    //        case (Some(debtToEquityValue), Some(roeValue), Some(actualValueToIntrinsicValuePercentValue)) =>
    //          debtToEquityValue < BigDecimal("0.5") &&
    //            roeValue > BigDecimal("0.15") &&
    //            actualValueToIntrinsicValuePercentValue <= BigDecimal(-20)
    //        case _ => false
    //      })
    //
    //    filteredStocks.foreach { stock => println(stock.toStringLine) }

    // ******************************************

    //    println("Stocks incomplete:\n")
    //    allStocks
    //      .filterNot(_.isComplete)
    //      .foreach { stock => println(s"${stock.missingFields} - ${stock.ticker}, ${stock.name}") }

    //    allStocks.foreach { case (ticker, name) => println(buildStock(ticker, name).toStringLine) }
    //    println(buildStock("AAPL", "Apple"))


    // TODO: Test intrinsic value calculation
    // result = 13.54123677723634959048464516707911
    //    val p = new util.ArrayList[BigDecimal]
    //    p.add(699 * (1 + 0.213))
    //    for (i <- 1 until 10) p.add(p.get(i - 1) * (1 + 0.213))
    //    println(util.Arrays.toString(p.toArray))
    //
    //    for (i <- 0 until p.size) p.set(i, p.get(i) / math.pow(1 + 0.079, i + 1))
    //    println(util.Arrays.toString(p.toArray))
    //
    //    val sum = p.asScala.sum
    //    println("sum = " + sum)
    //
    //    println("Intrinsic value: " + sum / 1039.22)
    //    println("Intrinsic value: " + calcIntrinsicValue(699, 0.213, 1.34, 1039.22))

  }
}
