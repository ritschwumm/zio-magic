ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.kitlangton"
ThisBuild / organizationName := "kitlangton"

val zioVersion = "1.0.3"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

lazy val dependencies =
  Seq(
    "dev.zio" %% "zio"          % zioVersion,
    "dev.zio" %% "zio-macros"   % zioVersion,
    "dev.zio" %% "zio-test"     % zioVersion % "test",
    "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
  )

lazy val root = (project in file("."))
  .settings(
    name := "zio-magic",
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    ),
    libraryDependencies ++= dependencies
  )
  .dependsOn(macros)

lazy val macros = (project in file("macros"))
  .settings(
    name := "zio-magic-macros",
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    ),
    libraryDependencies ++= dependencies,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"     % "2.2.0",
      "org.typelevel" %% "cats-free"     % "2.2.0",
      "org.scala-lang" % "scala-reflect" % "2.13.3"
    )
  )
