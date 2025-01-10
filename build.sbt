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

credentials += Credentials(
  "Artifactory Realm",
  "flow.jfrog.io",
  System.getenv("ARTIFACTORY_USERNAME"),
  System.getenv("ARTIFACTORY_PASSWORD"),
)

publishTo := {
  val host = "https://flow.jfrog.io/flow"
  if (isSnapshot.value) {
    Some("Artifactory Realm" at s"$host/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  } else {
    Some("Artifactory Realm" at s"$host/libs-release-local")
  }
}
version := "0.1.1"
