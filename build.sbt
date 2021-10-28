ThisBuild / scalaVersion := "2.13.6"
ThisBuild / organization := "com.validation"

lazy val validation = (project in file("."))
  .settings(
    name := "scala-fp-validation-framework"
  ).settings(
    libraryDependencies ++= Dependencies.validation
)