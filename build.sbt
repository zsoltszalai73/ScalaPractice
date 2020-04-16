name := "ScalaPractice"

version := "0.1"

//scalaVersion := "2.13.1"
scalaVersion := "2.12.10"

Compile/mainClass := Some("hu.sp.week3.AnsiPlayer")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.18"
