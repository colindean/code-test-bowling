import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      name     := "bowling",
      organization := "cx.cad",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Bowling Code Test",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      scalaCheck % Test
    )
  )
