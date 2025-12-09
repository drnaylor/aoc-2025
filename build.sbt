ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"

lazy val root = (project in file("."))
  .settings(
    name := "aoc-2025"
  )

libraryDependencies ++= List(
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.typelevel" %% "cats-effect" % "3.6.3",
  "org.scalactic" %% "scalactic" % "3.2.19",
  "org.scala-graph" %% "graph-core" % "2.0.3",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % "test"
)
