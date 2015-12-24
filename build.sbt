name := "SecureFacebook"

version := "1.0"

scalaVersion := "2.11.7"


resolvers += "spray repo" at "http://repo.spray.io"
resolvers += "spray nightlies repo" at "http://nightlies.spray.io"

val sprayVersion = "1.3.3"

libraryDependencies ++= Seq(
  "commons-codec" % "commons-codec" % "1.9",
  "org.apache.commons" % "commons-lang3" % "3.2",
  "com.typesafe.akka" %% "akka-actor" % "2.3.13",
  "io.spray" %% "spray-routing" % sprayVersion,
  "io.spray" %% "spray-client" % sprayVersion,
  "io.spray" %% "spray-can" % sprayVersion,
  "io.spray" %% "spray-json" % "1.3.1",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"
)