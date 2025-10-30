ThisBuild / scalaVersion := "3.5.0"
ThisBuild / organization := "com.github.kmizu"
ThisBuild / name := "treep"

lazy val root = (project in file(".")).settings(
  Compile / scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked"
  ),
  libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
)
