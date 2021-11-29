import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.jsEnv
import play.sbt.PlayImport.{PlayKeys, guice, specs2}
import sbt.Keys._
import sbt._


object Settings {

  private val moleculeVersion = "0.22.7"

  val common: Seq[Def.Setting[_]] = Seq(
    name := "molecule-admin",
    organization := "org.scalamolecule",
    version := "0.8",
    scalaVersion := "2.13.3",
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
      "com.lihaoyi" %%% "scalarx" % "0.4.2",
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0",
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    jsEnv in Test := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )

  val server: Seq[Def.Setting[_]] = common ++ Seq(
    name := "server",
    resolvers ++= Seq(
      "datomic" at "https://files.datomic.com/maven",
      "clojars" at "https://clojars.org/repo"
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalamolecule" %% "molecule" % moleculeVersion,
      "com.lihaoyi" %% "ammonite-ops" % "2.1.4",
      "com.datomic" % "datomic-free" % "0.9.5697",
      "org.webjars" % "jquery" % "3.5.1",
      "org.webjars.npm" % "popper.js" % "1.16.0",
      "org.webjars" % "bootstrap" % "4.5.0",
      "org.webjars.bower" % "open-iconic" % "1.1.1",
//      "org.webjars.npm" % "material-icons" % "0.3.1",
      "org.webjars" % "font-awesome" % "5.5.0",
      "org.webjars.bower" % "highlightjs" % "9.12.0",
      //      "org.atteo" % "evo-inflector" % "1.2.2", // English pluralizer
      //      "org.webjars.npm" % "jsdom" % "13.0.0",
      specs2 % Test,
      guice
    ).map(_.exclude("org.slf4j", "slf4j-nop")),
    PlayKeys.devSettings := Seq("play.server.http.port" -> "9001"),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

  val shared: Seq[Def.Setting[_]] = common ++ Seq(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % "0.9.1",
      "com.lihaoyi" %%% "autowire" % "0.3.2",
      "com.lihaoyi" %%% "utest" % "0.7.4",
      "io.suzaku" %%% "boopickle" % "1.3.2",
      ("org.scalamolecule" %%% "molecule" % moleculeVersion)
        .exclude("com.datomic", "datomic-free")
    ),
    updateSbtClassifiers / useCoursier := false,
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
}
