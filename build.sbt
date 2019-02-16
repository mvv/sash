import xerial.sbt.Sonatype._

inThisBuild(
  Seq(
    organization := "com.github.mvv.sash",
    version := "0.1-SNAPSHOT",
    homepage := Some(url("https://github.com/mvv/sash")),
    scmInfo := Some(ScmInfo(url("https://github.com/mvv/sash"), "scm:git@github.com:mvv/sash.git")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(id = "mvv",
                name = "Mikhail Vorozhtsov",
                email = "mikhail.vorozhtsov@gmail.com",
                url = url("https://github.com/mvv"))),
    sonatypeProjectHosting := Some(GitHubHosting("mvv", "sash", "mikhail.vorozhtsov@gmail.com")),
  ))

ThisBuild / publishTo := sonatypePublishTo.value
ThisBuild / publishMavenStyle := true

def isPriorTo2_13(version: String): Boolean =
  CrossVersion.partialVersion(version) match {
    case Some((2, minor)) => minor < 13
    case _                => false
  }

lazy val commonSettings = Seq(
  crossScalaVersions := Seq("2.12.8", "2.11.12", "2.13.0-M5"),
  scalaVersion := crossScalaVersions.value.head,
  scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xfatal-warnings"),
  scalacOptions ++= {
    if (isPriorTo2_13(scalaVersion.value)) {
      Nil
    } else {
      Seq("-Ymacro-annotations")
    }
  },
  libraryDependencies ++=
    Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value, "org.specs2" %% "specs2-core" % "4.4.1" % Test),
  libraryDependencies ++= {
    if (isPriorTo2_13(scalaVersion.value)) {
      Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
    } else {
      Nil
    }
  },
)

lazy val root = (project in file("."))
  .settings(skip in publish := true)
  .aggregate(core, cats, zio)

lazy val core = (project in file("./core"))
  .settings(commonSettings)
  .settings(name := "sash")

lazy val cats = (project in file("./cats"))
  .settings(commonSettings)
  .settings(name := "sash-cats", libraryDependencies ++= Seq("org.typelevel" %% "cats-core" % "1.6.0" % Provided))
  .dependsOn(core % "test->test;compile->compile")

lazy val zio = (project in file("./zio"))
  .settings(commonSettings)
  .settings(name := "sash-zio", libraryDependencies ++= Seq("org.scalaz" %% "scalaz-zio" % "0.6.1" % Provided))
  .dependsOn(core)
