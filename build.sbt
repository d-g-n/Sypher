name := "Sypher"

description := "A straightforward Neo4J lib for scala"

version := "0.1"

scalaVersion := "2.12.6"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
val circeVersion = "0.9.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser"% circeVersion,
  "com.typesafe.akka" %% "akka-actor" % "2.5.16",
  "com.typesafe.akka" %% "akka-stream" % "2.5.16",
  "com.typesafe.akka" %% "akka-http" % "10.1.4",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "org.java-websocket" % "Java-WebSocket" % "1.3.9",
  "org.scodec" %% "scodec-bits" % "1.1.5",
  "org.scodec" %% "scodec-core" % "1.10.3"
)