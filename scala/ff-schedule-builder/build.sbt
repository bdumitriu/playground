name := "ff-schedule-builder"

organization := "org.ffplanner"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-feature")

libraryDependencies += "org.scalaj" % "scalaj-time_2.10.0-M7" % "0.6" withSources()

libraryDependencies += "joda-time" % "joda-time" % "2.2" withSources()

libraryDependencies += "com.google.guava" % "guava" % "14.0" withSources()

libraryDependencies += "com.google.code.findbugs" % "jsr305" % "2.0.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test" withSources()

libraryDependencies += "junit" % "junit" % "4.11" % "test" withSources()

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.0" % "test" withSources()

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath + "/.m2/repository")))
