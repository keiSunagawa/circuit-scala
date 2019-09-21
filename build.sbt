import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "me.kerfume"

lazy val root = (project in file("."))
  .settings(
    name := "circuit-scala",
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.0.0",
      "org.typelevel" %% "cats-free" % "2.0.0",
      "com.chuusai" %% "shapeless" % "2.3.3"
        //      scalaTest % Test
    )
  )
