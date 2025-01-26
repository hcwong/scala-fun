import sbt.Keys.libraryDependencies

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

libraryDependencies ++= Seq("com.chuusai" %% "shapeless" % "2.3.3")

lazy val root = (project in file("."))
  .settings(
    name := "scala-fun"
  )

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
