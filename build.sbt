import xerial.sbt.Sonatype.sonatypeCentralHost


val commonSettings = Seq(
  ThisBuild / version := "0.1.0",
  ThisBuild / organization := "io.github.mikegirkin",
  usePgpKeyHex("50A47A03CCB118C0CE4DEB2FCD18B86413990F3F"),
  ThisBuild / publishMavenStyle := true,
  ThisBuild / pomIncludeRepository := { _ => false },
  ThisBuild / sonatypeCredentialHost := sonatypeCentralHost,
  ThisBuild / publishTo := sonatypePublishToBundle.value,
)

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(
    name := "ardeas",
    organizationName := "mikegirkin",
    organizationHomepage := Some(url("https://github.com/mikegirkin/")),
    homepage := Some(url("https://github.com/mikegirkin/ardeas")),
    description := "Client and server generator from openapi specs",
    licenses := List(
      "MIT" -> new URL("http://www.opensource.org/licenses/mit-license.php")
    ),
    developers := List(
      Developer(
        id = "mikegirkin",
        name = "Mike Girkin",
        email = "mikegirkin@gmail.com",
        url = url("https://github.com/mikegirkin/")
      )
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/mikegirkin/ardeas"),
        "scm:git@github.com:mikegirkin/ardeas.git"
      )
    ),

    scalaVersion := "3.3.1",

    libraryDependencies ++= Seq(
      "io.swagger.parser.v3" % "swagger-parser-v3" % "2.1.22",
      "com.github.scopt"     %% "scopt"            % "4.1.0",
      "org.typelevel"        %% "cats-core"        % "2.12.0",
      "com.monovore"         %% "decline"          % "2.4.1",


      "ch.qos.logback"       % "logback-classic"          % "1.5.6",
      "net.logstash.logback" % "logstash-logback-encoder" % "7.4",

      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    ),

    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation",
      "-Xfatal-warnings",
    )
  )
