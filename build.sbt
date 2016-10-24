name := "DropTableParser"

version := "0.1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.typesafe"     % "config"         % "1.3.1",
  "org.wvlet"        %% "wvlet-log"     % "1.1",
  "net.ruippeixotog" %% "scala-scraper" % "1.1.0"
)

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:implicitConversions")

mainClass in (Compile, run) := Some("com.harry0000.kancolle.ac.DropTableParser")
