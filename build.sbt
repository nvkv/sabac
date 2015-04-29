name := """sabac"""

version := "0.0.1"

scalaVersion := "2.11.5"
scalacOptions += "-deprecation"
scalacOptions += "-feature"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.yaml" % "snakeyaml" % "1.15"


// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.9"


fork in run := true
