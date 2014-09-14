name := "scala-maths"

version := "0.1.0.1"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
)


lazy val complex = project

lazy val rational = project

lazy val matrix = project dependsOn complex

lazy val `scala-maths` = project in file(".") aggregate (rational, complex, matrix)
