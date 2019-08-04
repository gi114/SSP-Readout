name := "SSP-Readout"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.6.0-M5",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.mockito" % "mockito-core" % "2.23.0" % Test)
