import sbtcrossproject.CrossPlugin.autoImport.crossProject


lazy val moleculeAdmin = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .configure(Settings.projects)
  .settings(Settings.shared)


lazy val moleculeAdminJS = moleculeAdmin.js
  .settings(Settings.js)
  .enablePlugins(
    ScalaJSPlugin,
    ScalaJSWeb, TzdbPlugin)


lazy val moleculeAdminJVM = moleculeAdmin.jvm
  .settings(
    Settings.jvm,
    PlayKeys.devSettings := Seq("play.server.http.port" -> "9001"),
    scalaJSProjects := Seq(moleculeAdminJS),
    pipelineStages in Assets := Seq(scalaJSPipeline),
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
//  .enablePlugins(MoleculePlugin).settings(moleculeSchemas := Seq("db/admin", "db/core", "db/integration", "db/migration"))


// loads the server project at sbt startup
onLoad in Global := (onLoad in Global).value andThen { s: State => "project moleculeAdminJVM" :: s }

