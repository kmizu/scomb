organization := "com.github.kmizu"

name := "scomb"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.1"

publishMavenStyle := true

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.6" % "test"
)


initialCommands in console += {
  Iterator().map("import "+).mkString("\n")
}
