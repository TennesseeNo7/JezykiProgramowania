name := "JezykiProgramowania"

version := "0.1"

scalaVersion := "2.12.8"


libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-actor" % "2.5.6",
	"com.typesafe.akka" %% "akka-testkit" % "2.5.6" % Test
)