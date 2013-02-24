name := "ff-schedule-builder"

organization := "org.ffplanner"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies += "org.scalaj" % "scalaj-time_2.10.0-M7" % "0.6" withSources()

libraryDependencies += "joda-time" % "joda-time" % "2.1" withSources()

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test" withSources()

libraryDependencies += "junit" % "junit" % "4.11" % "test" withSources()

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath + "/.m2/repository")))
