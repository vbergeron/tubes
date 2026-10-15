ThisBuild / organization := "tubes"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.9.0"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-explain",
  "-Xmax-inlines:64"
)

lazy val root = (project in file("."))
  .aggregate(core, benchmarks)
  .settings(
    name           := "tubes-root",
    publish / skip := true
  )

lazy val core = (project in file("core"))
  .settings(
    name                                   := "tubes",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test
  )

lazy val benchmarks = (project in file("benchmarks"))
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
  .settings(
    name           := "tubes-benchmarks",
    publish / skip := true
  )
