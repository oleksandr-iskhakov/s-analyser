package com.garallex.stocks

import java.net.URL
import java.util.concurrent.ConcurrentHashMap

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}

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


  //  def buildYahooFinanceUrl(ticker: String, prefix: String = null) = {
  //    val infoType = prefix match {
  //      case "ks" => "Key Statistics"
  //      case "ae" => "Analyst Estimates"
  //      case "cf" => "Cash Flow&annual"
  //      case _ => null
  //    }
  //
  //    val result = if (infoType != null)
  //      new URL(s"http://finance.yahoo.com/quote/$prefix?s=$ticker+${infoType.replace(' ', '+')}")
  //    else
  //      new URL(s"http://finance.yahoo.com/q?s=$ticker")
  //    println(result)
  //    result
  //  }

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

  //  def findElemContainsText(elements: Elements, searchString: String): Element = {
  //    for (elem <- elements.asScala) {
  //      if (elem.ownText() == searchString)
  //        return elem
  //    }
  //    null
  //  }

  //  def fetchCashFlowFromOperations(ticker: String) = {
  //    val doc = fetchWebPage(ticker, "key-statistics", buildYahooFinanceUrl)
  //    val elems = doc.body.getElementsContainingText("Operating Cash Flow")
  //    val td = gotoParentTagName(elems.get(0), "td")
  //    val element = td.parent.child(1).child(0)
  //    abbreviatedStringToBigDecimal(element.ownText)
  //  }

  //  def fetchCashFlowFromOperations(ticker: String) = {
  //    val doc = fetchWebPage(new URL(s"http://money.cnn.com/quote/financials/financials.html?dataSet=CFS&symb=$ticker"))
  //    val elements = doc.body.getElementsContainingOwnText("Net Cash Flow - Operating Activities")
  //    val td = elements.get(0)
  //    val element = td.parent.children.get(3)
  //    abbreviatedStringToBigDecimal(element.ownText)
  //  }

  def tryWrapper(ticker: String, func: String => BigDecimal) = Try {
    func(ticker)
  } match {
    case Success(value) => Some(value)
    case Failure(e) => None
  }

  def fetchPERatioCNN(ticker: String) = {
    val doc = fetchWebPageAsDocument(new URL(s"http://money.cnn.com/quote/quote.html?symb=$ticker"))
    val elements = doc.body.getElementsContainingOwnText("P/E ratio")
    val td = elements.get(0)
    val element = td.parent.children.get(1)
    abbreviatedStringToBigDecimal(element.ownText)
  }

  def fetchPERatio(ticker: String) = {
    val cnnResult = Future {
      Some(fetchPERatioCNN(ticker))
    }

    cnnResult fallbackTo Future {
      None
    }
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

  def fetchCashFlowFromOperations(ticker: String) = {
    val yahooResult = Future {
      Some(fetchCashFlowFromOperationsYahoo(ticker))
    }

    val cnnResult = Future {
      Some(fetchCashFlowFromOperationsCNN(ticker))
    }

    yahooResult fallbackTo cnnResult fallbackTo Future {
      None
    }
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

  def fetchLongTermGrowthRate(ticker: String) = {
    val url = new URL(s"https://query2.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=upgradeDowngradeHistory%2CrecommendationTrend%2CfinancialData%2CearningsHistory%2CearningsTrend%2CindustryTrend")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "{\"maxAge\":1,\"period\":\"+5y\",\"endDate\":null,\"growth\":{\"raw\":")
    BigDecimal(value)
  }

  //  def fetchLongTermGrowthRate(ticker: String) = {
  ////    val doc = fetchWebPage(ticker, "ae", buildYahooFinanceUrl)
  //    val url = new URL(s"https://query2.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=upgradeDowngradeHistory%2CrecommendationTrend%2CfinancialData%2CearningsHistory%2CearningsTrend%2CindustryTrend")
  //    val doc = fetchWebPage(url)
  //
  //    val elements = doc.body.getElementsContainingOwnText("Next 5 Years (per annum)")
  //    val element = elements.get(0).parent.children.get(1)
  //    BigDecimal(element.ownText.split('%').head) / BigDecimal("100")
  //  }

  //  def fetchSharesOutstanding(ticker: String) = {
  //    val doc = fetchWebPage(ticker, "ks", buildYahooFinanceUrl)
  //    val elements = doc.body.getElementsContainingOwnText("Shares Outstanding:")
  //    val element = elements.get(0).parent.children.get(1)
  //    abbreviatedStringToBigDecimal(element.ownText)
  //  }

  def fetchSharesOutstanding(ticker: String) = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"sharesOutstanding\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchBeta(ticker: String) = {
    val url = new URL(s"https://query1.finance.yahoo.com/v10/finance/quoteSummary/$ticker?modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents")
    val jsonString = fetchWebPageAsString(url)
    val value = findByKey(jsonString, "\"beta\":{\"raw\":")
    BigDecimal(value)
  }

  def fetchROERate(ticker: String): BigDecimal = {
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

  def fetchTotalDebtToEquityRate(ticker: String) = {
    val yahooResult = Future {
      Some(fetchTotalDebtToEquityRateYahoo(ticker))
    }

    val cnnResult = Future {
      Some(fetchTotalDebtToEquityRateReuters(ticker))
    }

    yahooResult fallbackTo cnnResult fallbackTo Future {
      None
    }
  }

  def fetchPrevClose(ticker: String) = {
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

  def buildStock(ticker: String, name: String, industry: String) = {
    val cashFlowFuture = fetchCashFlowFromOperations(ticker)

    val longTermGrowthFuture = Future {
      tryWrapper(ticker, fetchLongTermGrowthRate)
    }

    val betaFuture = Future {
      tryWrapper(ticker, fetchBeta)
    }

    val sharesOutstandingFuture = Future {
      tryWrapper(ticker, fetchSharesOutstanding)
    }

    val actualPriceFuture = Future {
      tryWrapper(ticker, fetchPrevClose)
    }

    val totalDebtToEquityRateFuture = fetchTotalDebtToEquityRate(ticker)

    val roeRateFuture = Future {
      tryWrapper(ticker, fetchROERate)
    }
    val peRatioFuture = fetchPERatio(ticker)

    val stockFuture = for {
      cashFlow <- cashFlowFuture
      longTermGrowth <- longTermGrowthFuture
      beta <- betaFuture
      sharesOutstanding <- sharesOutstandingFuture
      debtToEquity <- totalDebtToEquityRateFuture
      roe <- roeRateFuture
      peRatio <- peRatioFuture
      actualPrice <- actualPriceFuture
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
      actualPrice = actualPrice)

    Await.result(stockFuture, 1 minutes)
  }

  def main(args: Array[String]) = {
    //    val x = fetchTotalDebtToEquityRateReuters("PM")
    //    val x = fetchCashFlowFromOperationsCNN("AAPL")
    //    val x = fetchPERatioCNN("AAPL")

    //    val x = fetchLongTermGrowthRate("AAPL")
    //    val x = fetchBeta("AAPL")
    //    val x = fetchSharesOutstanding("AAPL")
    //    val x = calcIntrinsicValue("V")
    //    val x = fetchTotalDebtToEquityRate("AAPL")
    //    val x = fetchROERate("AAPL")
    //    val x = fetchPrevClose("AAPL")
    //    println(buildStock("ADS"))

    // MCD -??
    //    List("AAPL", "V", "MA")

    import Utils._

    println(formatLine("Ticker", "Name", "Industry", "D/E, %", "ROE, %", "P/E Ratio", "Actual price", "Intrinsic value", "A/I, %"))
    println()
    val tickers = fetchStockTickers()
      .toList
      .sortBy(_._3)
      .toParArray

    val allStocks = tickers
      .par
      .map { case (ticker, name, industry) =>
        val stock = buildStock(ticker, name, industry)
        println(stock.toStringLine)
        stock
      }
    println("\n")
    println("Screen is positive:\n")
    val filteredStocks =
      allStocks.filter(stock => (stock.debtToEquity, stock.roe, stock.actualValueToIntrinsicValuePercent()) match {
        case (Some(debtToEquityValue), Some(roeValue), Some(actualValueToIntrinsicValuePercentValue)) =>
          debtToEquityValue < BigDecimal("0.5") &&
            roeValue > BigDecimal("0.15") &&
            actualValueToIntrinsicValuePercentValue <= BigDecimal(-20)
        case _ => false
      })

    filteredStocks.foreach { stock => println(stock.toStringLine) }

    println("Stocks incomplete:\n")
    allStocks
      .filterNot(_.isComplete)
      .foreach { stock => println(s"${stock.missingFields} - ${stock.ticker}, ${stock.name}") }

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
