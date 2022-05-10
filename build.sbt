enablePlugins(ScalaJSPlugin)

name := "Nonograms"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.11.1"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.1.0"
