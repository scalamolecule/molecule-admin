import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import play.sbt.PlayImport.{guice, specs2}
import sbt.Keys._
import sbt._


object Settings {

  val projects: Project => Project = _.settings(
    name := "molecule-admin",
    organization := "org.scalamolecule",
    version := "0.6-SNAPSHOT",
    scalaVersion := "2.12.10",
    scalacOptions := Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked"
    )
  )

  val shared: Seq[Def.Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % "0.7.0",
      "com.lihaoyi" %%% "autowire" % "0.2.6",
      "com.lihaoyi" %%% "utest" % "0.7.1",
      "io.suzaku" %%% "boopickle" % "1.3.0"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

  val js: Seq[Def.Setting[_]] = Seq(
    name := "client",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.7",
      "com.lihaoyi" %%% "scalarx" % "0.4.0",
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-RC3",
      ("org.scalamolecule" %%% "molecule" % "0.20.1")
        .exclude("com.datomic", "datomic-free")
    )
//    testFrameworks += new TestFramework("utest.runner.Framework")
  )

  val jvm: Seq[Def.Setting[_]] = Seq(
    name := "server",
    resolvers ++= Seq(
      ("datomic" at "http://files.datomic.com/maven")
        .withAllowInsecureProtocol(true),
      ("clojars" at "http://clojars.org/repo")
        .withAllowInsecureProtocol(true)
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalamolecule" %% "molecule" % "0.20.1",
      "com.lihaoyi" %% "ammonite-ops" % "1.6.2",
      "com.datomic" % "datomic-free" % "0.9.5697",
      "org.webjars" %% "webjars-play" % "2.7.0",
      "org.webjars" % "jquery" % "3.3.1",
      "org.webjars.npm" % "popper.js" % "1.14.7",
      "org.webjars" % "bootstrap" % "4.3.1",
      "org.webjars.bower" % "angular-touch" % "1.6.9",
      "org.webjars.bower" % "angular-animate" % "1.6.9",
      "org.webjars.bower" % "open-iconic" % "1.1.1",
      "org.webjars" % "font-awesome" % "5.5.0",
      "org.webjars.bower" % "highlightjs" % "9.12.0",
//      "org.specs2" %% "specs2-core" % "4.8.0" % "test",
      specs2 % Test,
      guice
    ).map(_.exclude("org.slf4j", "slf4j-nop")) // necessary?
//    scalacOptions in Test ++= Seq("-Yrangepos"),
//    testFrameworks += new TestFramework("utest.runner.Framework")
  )
}
