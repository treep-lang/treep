scalaVersion := "3.7.3"
organization := "com.github.kmizu"
name := "treep"

<<<<<<< HEAD
lazy val root = (project in file(".")).settings(
  Compile / scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked"
  ),
  libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
=======
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked"
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.0.0" % Test
>>>>>>> aa17f4483079e0ab8e8dc740702d56c5122247d4
)
