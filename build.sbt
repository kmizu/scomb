organization := "com.github.kmizu"

name := "scomb"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.8"

publishMavenStyle := true

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)


initialCommands in console += {
  Iterator().map("import "+).mkString("\n")
}
