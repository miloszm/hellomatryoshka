name := """hello-scala"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "com.slamdata" %% "matryoshka-core" % "0.18.3"


fork in run := true