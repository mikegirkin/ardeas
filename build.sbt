scalaVersion := "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "ardeas",
    version := "0.1.0-SNAPSHOT",

    libraryDependencies ++= Seq(
      "io.swagger.parser.v3" % "swagger-parser-v3" % "2.1.12",
      "com.github.scopt"     %% "scopt"            % "4.1.0",
      "org.typelevel"        %% "cats-core" % "2.9.0",

      "ch.qos.logback"       % "logback-classic"          % "1.4.7",
      "net.logstash.logback" % "logstash-logback-encoder" % "7.3",

      "org.scalatest" %% "scalatest" % "3.2.15" % Test
    ),

    scalacOptions ++= Seq(
      "-unchecked",
      "-Xfatal-warnings",
    )
  )
