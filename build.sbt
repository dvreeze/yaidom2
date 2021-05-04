
// Building both for JVM and JavaScript runtimes.

// To convince SBT not to publish any root level artifacts, I had a look at how scala-java-time does it.
// See https://github.com/cquiroz/scala-java-time/blob/master/build.sbt as a "template" for this build file.


// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val scalaVer = "2.13.5"
val crossScalaVer = Seq(scalaVer, "3.0.0-RC3")

ThisBuild / description  := "Extensible XML query API with multiple DOM-like implementations, 2nd generation"
ThisBuild / organization := "eu.cdevreeze.yaidom2"
ThisBuild / version      := "0.14.0-SNAPSHOT"

ThisBuild / scalaVersion       := scalaVer
ThisBuild / crossScalaVersions := crossScalaVer

ThisBuild / scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  case (Some((3, _))) =>
    Seq("unchecked", "-source:3.0-migration")
  case _ =>
    Seq("-Wconf:cat=unused-imports:w,cat=unchecked:w,cat=deprecation:w,cat=feature:w,cat=lint:w")
})

ThisBuild / Test / publishArtifact := false
ThisBuild / publishMavenStyle := true

ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) {
    Some("snapshots" at nexus + "content/repositories/snapshots")
  } else {
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
}

ThisBuild / pomExtra := pomData
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / libraryDependencies += "org.scala-lang.modules" %%% "scala-xml" % "2.0.0-RC1"

ThisBuild / libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.8" % Test

ThisBuild / libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-15" % "3.2.8.0" % Test

lazy val root = project.in(file("."))
  .aggregate(yaidom2JVM, yaidom2JS)
  .settings(
    name                 := "yaidom2",
    // Thanks, scala-java-time, for showing us how to prevent any publishing of root level artifacts:
    // No, SBT, we don't want any artifacts for root. No, not even an empty jar.
    publish              := {},
    publishLocal         := {},
    publishArtifact      := false,
    Keys.`package`       := file(""))

lazy val yaidom2 = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .jvmSettings(
    // By all means, override this version of Saxon if needed, possibly with a Saxon-EE release!

    libraryDependencies += "net.sf.saxon" % "Saxon-HE" % "9.9.1-8",

    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.15.4" % Test,

    mimaPreviousArtifacts := Set("eu.cdevreeze.yaidom2" %%% "yaidom2" % "0.12.0")
  )
  .jsSettings(
    // Do we need this jsEnv?
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),

    libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "1.1.0").cross(CrossVersion.for3Use2_13), // Hopefully soon not needed anymore

    Test / parallelExecution := false,

    mimaPreviousArtifacts := Set("eu.cdevreeze.yaidom2" %%% "yaidom2" % "0.12.0")
  )

lazy val yaidom2JVM = yaidom2.jvm

lazy val yaidom2JS = yaidom2.js

lazy val pomData =
  <url>https://github.com/dvreeze/yaidom2</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
      <comments>Yaidom2 is licensed under Apache License, Version 2.0</comments>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:git@github.com:dvreeze/yaidom2.git</connection>
    <url>https://github.com/dvreeze/yaidom2.git</url>
    <developerConnection>scm:git:git@github.com:dvreeze/yaidom2.git</developerConnection>
  </scm>
  <developers>
    <developer>
      <id>dvreeze</id>
      <name>Chris de Vreeze</name>
      <email>chris.de.vreeze@caiway.net</email>
    </developer>
  </developers>
