name := "lectures"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
        "com.netflix.rxjava" % "rxjava-scala" % "0.15.1" withSources(),
        "org.scalatest" % "scalatest_2.10" % "2.0" withSources(),
        "junit" % "junit" % "4.11" withSources(),
        "org.scala-lang.modules" %% "scala-async" % "0.9.0-M4" withSources(),
        "com.squareup.retrofit" % "retrofit" % "1.2.2" withSources(),
        "com.typesafe.akka" %% "akka-actor" % "2.2.1" withSources(),
        "com.typesafe.akka" %% "akka-actor" % "2.2.3" withSources(),
        "com.typesafe.akka" %% "akka-testkit" % "2.2.3" withSources(),
        "com.ning" % "async-http-client" % "1.7.22" withSources(),
        "ch.qos.logback" % "logback-classic" % "1.0.7"
)
