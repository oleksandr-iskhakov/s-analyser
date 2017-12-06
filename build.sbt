name := "analyser"

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.jsoup" % "jsoup" % "1.9.2",
  "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.4",
  "org.json4s" %% "json4s-native" % "3.5.3",
  "org.apache.httpcomponents" % "httpclient" % "4.5.3",
  "org.json4s" %% "json4s-jackson" % "3.5.3",
  "org.mongodb.scala" %% "mongo-scala-driver" % "2.1.0"
)

resolvers ++= Seq(
  "Sonatype OSS" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/",
  "Maven Repository" at "http://repo1.maven.org/maven2/",
  Resolver.mavenLocal
)

scalacOptions += "-feature"
scalacOptions += "-language:postfixOps"
