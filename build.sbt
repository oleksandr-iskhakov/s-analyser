name := "analyser"

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
//  "com.yahoofinance-api" % "YahooFinanceAPI" % "3.5.0",
  "org.jsoup" % "jsoup" % "1.9.2",
  "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.4",
  "org.json4s" %% "json4s-native" % "3.5.3",
//  "edu.stanford.nlp" % "stanford-corenlp" % "3.7.0",
//  "edu.stanford.nlp" % "stanford-corenlp" % "3.7.0" classifier "models",
  "org.apache.httpcomponents" % "httpclient" % "4.5.3"
)

resolvers ++= Seq(
  "Sonatype OSS" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/",
  "Maven Repository" at "http://repo1.maven.org/maven2/",
  Resolver.mavenLocal
)

scalacOptions += "-feature"
scalacOptions += "-language:postfixOps"
