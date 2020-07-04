import xerial.sbt.Sonatype._

inThisBuild(
  Seq(
    organization := "com.github.mvv.sash",
    version := "0.1-SNAPSHOT", // next is M6
    homepage := Some(url("https://github.com/mvv/sash")),
    scmInfo := Some(ScmInfo(url("https://github.com/mvv/sash"), "scm:git@github.com:mvv/sash.git")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(id = "mvv",
                name = "Mikhail Vorozhtsov",
                email = "mikhail.vorozhtsov@gmail.com",
                url = url("https://github.com/mvv"))),
    sonatypeProjectHosting := Some(GitHubHosting("mvv", "sash", "mikhail.vorozhtsov@gmail.com"))
  ))

ThisBuild / publishTo := sonatypePublishTo.value
ThisBuild / publishMavenStyle := true

lazy val sonatypeBundleReleaseIfNotSnapshot: Command = Command.command("sonatypeBundleReleaseIfNotSnapshot") { state =>
  val extracted = Project.extract(state)
  if (extracted.get(isSnapshot)) {
    val log = extracted.get(sLog)
    log.info("Snapshot version, doing nothing")
    state
  } else {
    Command.process("sonatypeBundleRelease", state)
  }
}

lazy val scala2_11 = "2.11.12"
lazy val scala2_12 = "2.12.11"
lazy val scala2_13 = "2.13.2"

ThisBuild / scalaVersion := scala2_13

def isPriorTo2_13(version: String): Boolean =
  CrossVersion.partialVersion(version) match {
    case Some((2, minor)) => minor < 13
    case _                => false
  }

lazy val commonSettings = Seq(
  crossScalaVersions := Seq(scala2_11, scala2_12, scala2_13),
  scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xfatal-warnings"),
  scalacOptions ++= {
    if (isPriorTo2_13(scalaVersion.value)) {
      Nil
    } else {
      Seq("-Ymacro-annotations")
    }
  },
  libraryDependencies ++=
    Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value, "org.specs2" %% "specs2-core" % "4.10.0" % Test),
  libraryDependencies ++= {
    if (isPriorTo2_13(scalaVersion.value)) {
      Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
    } else {
      Nil
    }
  }
)

lazy val sash = (project in file("."))
  .settings(
    crossScalaVersions := Nil,
    skip in publish := true,
    sonatypeProfileName := "com.github.mvv",
    sonatypeSessionName := s"Sash_${version.value}",
    commands += sonatypeBundleReleaseIfNotSnapshot
  )
  .aggregate(core, cats, zio)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(name := "sash")

lazy val cats = (project in file("cats"))
  .settings(commonSettings)
  .settings(name := "sash-cats", libraryDependencies ++= Seq("org.typelevel" %% "cats-core" % "2.0.0" % Provided))
  .dependsOn(core % "test->test;compile->compile")

lazy val zio = (project in file("zio"))
  .settings(commonSettings)
  .settings(
    name := "sash-zio",
    libraryDependencies ++= {
      val zioVersion = "1.0.0-RC21-2"
      Seq("dev.zio" %% "zio" % zioVersion % Provided, "dev.zio" %% "zio-streams" % zioVersion % Provided)
    }
  )
  .dependsOn(core)
