// run `sbt update-classifiers' to get the sources

name := "ff-schedule-builder"

organization := "org.ffplanner"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-feature")

libraryDependencies += "org.scalaj" % "scalaj-time_2.10.2" % "0.7"

libraryDependencies += "joda-time" % "joda-time" % "2.3"

libraryDependencies += "com.google.guava" % "guava" % "15.0"

libraryDependencies += "com.google.code.findbugs" % "jsr305" % "2.0.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.2" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.3" % "test"

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath + "/.m2/repository")))
