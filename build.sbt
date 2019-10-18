
// Building both for JVM and JavaScript runtimes.

// To convince SBT not to publish any root level artifacts, I had a look at how scala-java-time does it.
// See https://github.com/cquiroz/scala-java-time/blob/master/build.sbt as a "template" for this build file.


// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val scalaVer = "2.13.1"
val crossScalaVer = Seq(scalaVer)

lazy val commonSettings = Seq(
  name         := "yaidom2",
  description  := "Extensible XML query API with multiple DOM-like implementations, 2nd generation",
  organization := "eu.cdevreeze.yaidom2",
  version      := "0.6.1",

  scalaVersion       := scalaVer,
  crossScalaVersions := crossScalaVer,

  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "-Xlint"),

  Test / publishArtifact := false,
  publishMavenStyle := true,

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },

  pomExtra := pomData,
  pomIncludeRepository := { _ => false },

  libraryDependencies += "org.scala-lang.modules" %%% "scala-xml" % "1.2.0",

  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.8" % "test"
)

lazy val root = project.in(file("."))
  .aggregate(yaidom2JVM, yaidom2JS)
  .settings(commonSettings: _*)
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
  .settings(commonSettings: _*)
  .jvmSettings(
    // By all means, override this version of Saxon if needed, possibly with a Saxon-EE release!

    libraryDependencies += "net.sf.saxon" % "Saxon-HE" % "9.9.1-5",

    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.14.2" % "test"
  )
  .jsSettings(
    // Do we need this jsEnv?
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),

    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7",

    Test / parallelExecution := false
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
