name := "analyser"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.yahoofinance-api" % "YahooFinanceAPI" % "3.3.0",
  "org.jsoup" % "jsoup" % "1.9.2",
  "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4"
)

resolvers ++= Seq(
  "Sonatype OSS" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/",
  "Maven Repository" at "http://repo1.maven.org/maven2/",
  Resolver.mavenLocal
)