import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import play.sbt.PlayImport.{PlayKeys, guice, specs2}
import sbt.Keys._
import sbt._


object Settings {

  val common: Seq[Def.Setting[_]] = Seq(
    name := "molecule-admin",
    organization := "org.scalamolecule",
    version := "0.7",
    scalaVersion := "2.13.2",
    scalacOptions := Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
      "-language:postfixOps"
    )
  )

  val client: Seq[Def.Setting[_]] = common ++ Seq(
    name := "client",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "1.0.0",
      "com.lihaoyi" %%% "scalarx" % "0.4.2"
    )
  )

  val server: Seq[Def.Setting[_]] = common ++ Seq(
    name := "server",
    resolvers ++= Seq(
      ("datomic" at "http://files.datomic.com/maven")
        .withAllowInsecureProtocol(true),
      ("clojars" at "http://clojars.org/repo")
        .withAllowInsecureProtocol(true)
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalamolecule" %% "molecule" % "0.22.3-SNAPSHOT",
      "com.lihaoyi" %% "ammonite-ops" % "2.1.4",
      "com.datomic" % "datomic-free" % "0.9.5697",
      "org.webjars" % "jquery" % "3.3.1",
      "org.webjars.npm" % "popper.js" % "1.14.7",
      "org.webjars" % "bootstrap" % "4.3.1",
      "org.webjars.bower" % "angular-touch" % "1.6.9",
      "org.webjars.bower" % "angular-animate" % "1.6.9",
      "org.webjars.bower" % "open-iconic" % "1.1.1",
      "org.webjars" % "font-awesome" % "5.5.0",
      "org.webjars.bower" % "highlightjs" % "9.12.0",

      //      "org.atteo" % "evo-inflector" % "1.2.2", // English pluralizer
      //      "org.webjars.npm" % "jsdom" % "13.0.0",
      specs2 % Test,
      guice
    ).map(_.exclude("org.slf4j", "slf4j-nop")),
    PlayKeys.devSettings := Seq("play.server.http.port" -> "9001")
  )

  val shared: Seq[Def.Setting[_]] = common ++ Seq(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % "0.9.1",
      "com.lihaoyi" %%% "autowire" % "0.3.2",
      "com.lihaoyi" %%% "utest" % "0.7.4",
      "io.suzaku" %%% "boopickle" % "1.3.2",
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0",
      ("org.scalamolecule" %%% "molecule" % "0.22.3-SNAPSHOT")
        .exclude("com.datomic", "datomic-free")
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
}
