name := "apibuilder-commons"

organization := "io.apibuilder"

ThisBuild / scalaVersion := "3.4.2"

lazy val allScalacOptions = Seq(
  "-feature",
  "-Xfatal-warnings"
)

lazy val root = project
  .in(file("."))
  .settings(
    testOptions += Tests.Argument("-oDF"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    ),
    scalacOptions ++= allScalacOptions,
  )
