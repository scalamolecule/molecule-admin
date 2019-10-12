import sbtcrossproject.CrossPlugin.autoImport.crossProject


lazy val moleculeAdmin = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .configure(Settings.projects)
  .settings(Settings.shared)


lazy val moleculeAdminJS = moleculeAdmin.js
  .settings(Settings.js)
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb, TzdbPlugin)


lazy val moleculeAdminJVM = moleculeAdmin.jvm
  .settings(
    Settings.jvm,
    PlayKeys.devSettings := Seq("play.server.http.port" -> "9001"),
    //    pipelineStages := Seq(digest, gzip),

    // https://github.com/vmunier/sbt-web-scalajs
    scalaJSProjects := Seq(moleculeAdminJS),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    //  compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value, // sbt-web-scalajs: Scala.js continuous compilation
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin) // use the standard directory layout instead of Play's custom
//  .enablePlugins(MoleculePlugin).settings(moleculeSchemas := Seq("db/admin", "db/core", "db/integration", "db/migration"))


// loads the server project at sbt startup
onLoad in Global := (onLoad in Global).value andThen { s: State => "project moleculeAdminJVM" :: s }

